Description of data contained in each table
--------------------------------------------

genes.csv
---------
A lookup table for mapping gene ensembl ids to their corresponding hgnc symbol (hugo symbol).
This was created using biomart and only contains the genes which appear in the rest of the data.


donors_clean.csv
----------------
Information on each of the 89 donors used in the visualisations
There are several samples taken from each donor. For these 89, one tumour sample is matched against one normal sample for variant analysis.
In the cnv analysis, multiple tumour samples may have been used in analysis.


struct_clean.csv
----------------
Large variation in the genome (I think >200bp is the convention here) and chromosomal rearrangement.
All structural variants for all donors are found in this table.
Each row is one variant.

- Columns
	- icgc_donor_id: 
		The donor this variant was found for.
	- icgc_specimen_id: 
		One-to-one with icgc_donor_id.
    - icgc_sample_id: 
    	One-to-one with icgc_donor_id.
    - annotation: 
    	Info on type of structural variantion (DEL (deletion), INS (insertion), INV (inversion), ITX (intra-chromosomal-translocation), CTX (inter-chromosomal-translocation)) (currently not used in visualisation, structural variants just split by intra / inter-chromosomal)
    - chr_from, chr_bkpt_from: 
    	Where in the genome the structural variant is from.
    - chr_to, chr_bkpt_to: 
    	Where in the genome the structural variant 'moves' to.

Note the chr_from, chr_from_bkpt, chr_to, chr_to_bkpt columns have a slightly flexible interpretation (i.e. a DEL (deletion) is deleted, not moved). Also chromosomal rearrangements can be reciprocal or not and this currently isn't shown in the visualisations. There are some obvious 'pairs' where parts of the genome have 'swapped' but it's not clear whether the process used in the variant calling is designed to to check for this or not.

snp_clean.csv
-------------
Small variation in the genome.
All small variants for all donors in this table.
Each row is one effect on a transcript. One variant can affect multiple transcripts.

- Columns
	- icgc_donor_id: 
		The donor this variant was found for.
	- icgc_specimen_id: 
		One-to-one with icgc_donor_id.
    - icgc_sample_id: 
    	One-to-one with icgc_donor_id.
	- matched_sample_id: 
		The normal sample that was matched with the tumour sample (icgc_sample_id) to find the somatic variants. (One-to-one with icgc_sample_id due to criteria used to select donors)
	- chromosome, chromosome_start, chromosome_end:
		Position in the genome of this variant. 
	    Mostly chromosome_start = chromosome_end but some variants may involve several base pairs (<=200).
	- mutation_type:
		Whether this variant is a single nucleotide polymorphism, a deletion or an insertion.
	- reference_genome_allele, mutated_from_allele, mutated_to_allele:
		Not used in current visualisations.
	- consequence_type: 
		The effect on to the transcript.
	- gene_affected:
        The ensembl id of the gene in which the variant has been found.
    - transcript_affected:
    	The ensembl id of the transcript in which the variant has been found.


cnv_clean.csv
-------------
Copy number variation in the genome.
The workflow for getting to this data seems to be less well established than for the other variants.
For some donors, multiple tumour samples have been compared against a single normal sample.

- Columns
	- icgc_donor_id: 
		The donor this variant was found for.
	- icgc_specimen_id: 
		Not necessarily one-to-one with icgc_donor_id.
    - icgc_sample_id: 
    	Not necessarily one-to-one with icgc_donor_id.
	- matched_sample_id: 
		The normal sample that was matched with the tumour sample (icgc_sample_id) to find the somatic variants.
    - mutation_type:
        Whether this variant is a gain, loss, or LOH cnv.
    - copy_number:
    	The number of copies of this part of the genome in the sample (in normal diploid cells this should be 2). Can be 2 and still a variant if there is LOH.
    - segment_mean:
    	Value which is used in copy number analysis. 
    	One of the factors considered when deriving copy number.
    	Not in the current version of the visualisations although could be an alternative to plotting the discrete copy numbers.
    	Decided on plotting the discrete copy numbers since the process to get to them is complicated and involves more than just using the segment mean (we don't seem to have all that would be required to derive the copy numbers in the raw data).
    	Also, although there is a positive correlation between segment mean and copy number, segment_mean_x > segment_mean_y !imply copy_number_x > copy_number_y
    - chromosome, chromosome_start, chromosome_end:
    	Position of variant in the genome.
    	Current visualisations use the midpoint of these sections since the lengths of the individual sections are negligible on the scale currently used. Combining adjacent sections or zooming in on a part of the genome could change this though so may want to be treated as a section rather than points, as in the current visualisations.






