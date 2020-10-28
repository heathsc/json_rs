# json_rs

Barebones pure rust parser for JSON inspired by the C JSON parser JSMN.  The intended use of json_rs is for processing large JSON files where most of the content is not required (the original use case was for parsing JSON files with SNP information from dbSNP: 30-40k of data per SNP and hundreds of millions of SNPs, and where we only want a few items of information from each SNP).  In these circumstances, copying all of the input data into Rust structs is time consuming and wasteful and most of the data will not be used.  With json_rs (as with JSMN) the aim is to generate a list of parsed tokens where each token points to a slice of the input string.  Only the required data then needs to be copied into Rust structs.

The parse is strict (by intention).  It has been tested on the JSON test files in https://github.com/nst/JSONTestSuite with a perfect score on the obligatory pass and fail tests (the tests that fail are files that are for various reasons not valid UTF8 and are rejected by the standard Rust text processing machinery).

