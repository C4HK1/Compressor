#include <cstdlib>
#include <cstdio>
#include <vector>
#include <algorithm>
#include <iostream>
#include <cmath>
#include <limits.h>

#include "ari.h"

using namespace std;

class BitStream
{
private:
    unsigned char rbyte;
    int rbit_in_byte;
    unsigned char wbyte;
    int wbit_in_byte;

public:
    FILE *fin;
    FILE *fout;
    bool is_empty;

    void operator=(const BitStream &bitstream)
    {
        this->fin = bitstream.fin;
        this->fout = bitstream.fout;
        this->is_empty = bitstream.is_empty;
    }

    BitStream()
    {
        this->rbit_in_byte = -1;
        this->wbit_in_byte = 0;
        this->wbyte = 0;
        this->rbyte = 0;
        this->is_empty = 0;
    }

    BitStream(char *ifile, char *ofile)
    {
        this->fin = fopen(ifile, "rb");
        this->fout = fopen(ofile, "wb");
        this->rbit_in_byte = -1;
        this->wbit_in_byte = 0;
        this->wbyte = 0;
        this->rbyte = 0;
        this->is_empty = 0;
    }

    bool read_bit()
    {
        bool bit = 1;

        if (this->rbit_in_byte < 0)
        {
            if (fread(&this->rbyte, sizeof(char), 1, this->fin) < 1)
            {
                this->is_empty = 1;
                return 0;
            }
            this->rbit_in_byte = 7;
        }
        bit &= this->rbyte >> this->rbit_in_byte;
        this->rbit_in_byte--;
        return bit;
    }

    void write_bit(bool bit)
    {
        this->wbyte <<= 1;
        this->wbyte |= bit;
        this->wbit_in_byte++;

        if (this->wbit_in_byte == 8)
        {
            fwrite(&this->wbyte, 1, 1, this->fout);
            this->wbit_in_byte = 0;
        }
    }

    void write_byte(unsigned char byte)
    {
        for (int i = 7; i >= 0; --i)
        {
            this->write_bit((byte >> i) & 1);
        }
    }

    void close()
    {
        if (this->wbyte > 0)
        {
            for (int i = this->wbit_in_byte; i < 8; ++i)
            {
                this->write_bit(0);
            }
        }
        fclose(this->fin);
        fclose(this->fout);
    }
};

class FrequencyTable
{
private:
public:
    unsigned long long divisor;
    vector<vector<int>> table;

    FrequencyTable()
    {
        vector<int> arr(1, 0);
        for (int i = 0; i < 256; ++i)
        {
            this->table.push_back({i, 1, i + 1});
        }
        this->divisor = 256;
    }

    int find_el_by_first_id(unsigned char el)
    {
        for (int i = 0; i < 256; ++i)
        {
            if (this->table[i][0] == el)
            {
                return i;
            }
        }
        return 0;
    }

    int find_el_by_third_id(unsigned long long el, unsigned long long r, unsigned long long l)
    {
        for (int i = 1; i < 256; ++i)
        {
            if ((el >= (r - l + 1) * this->table[i - 1][2] / this->divisor) &&
                (el < (r - l + 1) * this->table[i][2] / this->divisor))
            {
                return i;
            }
        }
        return 0;
    }

    void update(unsigned char byte)
    {
        this->table[this->find_el_by_first_id(byte)][1]++;

        this->divisor = 0;
        for (int i = 0; i < 256; ++i)
        {
            this->divisor += this->table[i][1];
            this->table[i][2] = this->divisor;
        }
    }
};

class ArithmeticCompressor
{
private:
    BitStream bitstream;
    FrequencyTable frequency_table;
    unsigned long long l = 0;
    unsigned long long r = UINT_MAX;
    unsigned long long qtr = (r + 1) / 4;
    unsigned long long half = 2 * qtr;
    unsigned long long three_qtr = 3 * qtr;
    unsigned long long bits_to_follow = 0;

public:
    unsigned long long value;

    ArithmeticCompressor(BitStream &bitstream, FrequencyTable &frequency_table)
    {
        this->bitstream = bitstream;
        this->frequency_table = frequency_table;
        this->value = 0;
    }

    void bits_plus_follow(bool bit)
    {
        this->bitstream.write_bit(bit);
        for (int i = this->bits_to_follow; i > 0; --i)
        {
            this->bitstream.write_bit(!bit);
        }
        this->bits_to_follow = 0;
    }

    void encode_byte(unsigned char byte)
    {
        int j = this->frequency_table.find_el_by_first_id(byte);
        unsigned long long l = 0;
        if (j > 0)
        {
            l = this->l + (this->r - this->l + 1) * this->frequency_table.table[j - 1][2] / this->frequency_table.divisor;
        }
        this->r = this->l + (this->r - this->l + 1) * this->frequency_table.table[j][2] / this->frequency_table.divisor - 1;
        this->l = l;

        while (true)
        {
            if (this->r < this->half)
            {
                this->bits_plus_follow(0);
            }
            else if (this->l >= this->half)
            {
                this->bits_plus_follow(1);
                this->l -= this->half;
                this->r -= this->half;
            }
            else if ((this->l >= this->qtr) && (this->r < this->three_qtr))
            {
                this->bits_to_follow++;
                this->l -= qtr;
                this->r -= qtr;
            }
            else
            {
                break;
            }

            this->l *= 2;
            this->r *= 2;
            this->r++;
        }

        this->frequency_table.update(byte);
    }

    char decode_byte()
    {
        int j = this->frequency_table.find_el_by_third_id(this->value - this->l, this->r, this->l);
        char byte = this->frequency_table.table[j][0];
        unsigned long long l = 0;
        if (j > 0)
        {
            l = this->l + (this->r - this->l + 1) * this->frequency_table.table[j - 1][2] / this->frequency_table.divisor;
        }
        this->r = this->l + (this->r - this->l + 1) * this->frequency_table.table[j][2] / this->frequency_table.divisor - 1;
        this->l = l;

        while (true)
        {
            if (this->r < this->half)
            {
            }
            else if (this->l >= this->half)
            {
                this->value -= this->half;
                this->l -= this->half;
                this->r -= this->half;
            }
            else if ((this->l >= this->qtr) && (this->r < this->three_qtr))
            {
                this->value -= this->qtr;
                this->l -= this->qtr;
                this->r -= this->qtr;
            }
            else
            {
                break;
            }
            this->l *= 2;
            this->r *= 2;
            this->r++;

            unsigned long long bit = this->bitstream.read_bit();

            if (this->bitstream.is_empty)
            {
                bit = 0;
            }
            this->value += this->value + bit;
        }
        this->frequency_table.update(byte);
        return byte;
    }

    void finish()
    {
        this->bits_to_follow++;

        if (this->l < this->qtr)
        {
            this->bits_plus_follow(0);
        }
        else
        {
            this->bits_plus_follow(1);
        }
    }

    ~ArithmeticCompressor()
    {
        this->bitstream.close();
    }
};

void compress_ari(char *ifile, char *ofile)
{
    BitStream bs(ifile, ofile);
    FrequencyTable table;
    ArithmeticCompressor compressor(bs, table);
    unsigned char byte;

    int counter = 0;
    fwrite(&counter, sizeof(int), 1, bs.fout);

    while (fread(&byte, sizeof(byte), 1, bs.fin))
    {
        compressor.encode_byte(byte);
        ++counter;
    }

    compressor.finish();

    fseek(bs.fout, 0, SEEK_SET);
    fwrite(&counter, sizeof(counter), 1, bs.fout);
    fseek(bs.fout, 0, SEEK_END);
}

void decompress_ari(char *ifile, char *ofile)
{
    BitStream bs(ifile, ofile);
    FrequencyTable table;
    ArithmeticCompressor compressor(bs, table);

    unsigned long long value = 0;
    int count;
    fread(&count, sizeof(int), 1, bs.fin);
    for (int i = 0; i < 4; ++i)
    {
        value <<= 8;
        unsigned char byte;
        fread(&byte, sizeof(byte), 1, bs.fin);
        if (!bs.is_empty)
        {
            value |= byte;
        }
    }
    compressor.value = value;

    for (int i = 0; i < count; ++i)
    {
        bs.write_byte((unsigned char)compressor.decode_byte());
    }
}