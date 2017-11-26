# Haskell Image Encrypt

Nicholas Maltbie

## Description

This project will use haskell to encrypt files, text, and images into other 
images using stenography by modifying bit values for images. In order to do 
this, I will utilize a few libraries to edit the images and encrypt values from 
files. 

## Purpose

This is a project for CS 4003, University of Cincinnati, instructor John Franco.

## Libraries

* [Haskell Image Processing (HIP) Library](https://hackage.haskell.org/package/hip)
* [The cipher-aes package](https://hackage.haskell.org/package/cipher-aes)

## Specific Stenographic Methods

1. [Basic least significant bit embedding without encryption][5]
2. [Basic least significant bit embedding with AES encryption][5]
2. [Rajkumar Yadav, et al.][2]
3. [Mamta Juneja, et al.][3]
4. [Shamim Ahmed Laskar, et al.][4]

## References

> [C.P.Sumathi1, T.Santanam2 and G.Umamaheswari3. "A Study of Various Steganographic Techniques Used for Information Hiding"][1]

> [Rajkumar Yadav, Ravi Saini & Gaurav Chawla. "A Novel Approach For Image Steganography In Spatial DoMain Using Last Two Bits of Pixel Value"][2]

> [Mamta Juneja and Parvinder Singh Sandhu. "A New Approach for Information Security using an Improved Steganography Technique"][3]

> [Shamim Ahmed Laskar and Kattamanchi Hemachandran. "Steganography based on random pixel selection for efficient data hiding"][4]

> [Matteo Fortini "Least Significant Bits (LSB) insertion"][5]

[1]: https://arxiv.org/pdf/1401.5561.pdf
[2]: http://www.cscjournals.org/manuscript/Journals/IJS/Volume5/Issue2/IJS-77.pdf
[3]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.1002.2544&rep=rep1&type=pdf
[4]: https://www.academia.edu/3216126/STEGANOGRAPHY_BASED_ON_RANDOM_PIXEL_SELECTION_FOR_EFFICIENT_DATA_HIDING
[5]: http://www.lia.deis.unibo.it/Courses/RetiDiCalcolatori/Progetti98/Fortini/lsb.html

