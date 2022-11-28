# Processing New South Wales (Australia) Admitted Patient Data to Create Encounters from Episodes

_Matthew Miller_

  

With advice from _Blanca Gallego, Louisa Jorm_ and _Dami Sotade_

  

  

**Overview**

  

The admitted patient data collection (APDC) contains patient care episodes rather than admissions. An admission needs to be created by combining the appropriate patient care episodes. There are a number of fields that could be helpful in determining when the end of a patient care episode corresponds to the end of an admission, for example 'mode of separation' is recorded as "Discharged by Hospital". Unfortunately, the documentation of a "Discharge" is not always helpful. For example,

patients can be transferred between hospitals but are recorded as "Discharged from Hospital" on transfer although their inpatient episode continues as a second hospital. Patients on renal dialysis are always documented as being discharged from hospital at the end of their dialysis treatment, even if they are an inpatient. In addition, some multiple-day episodes are nested parts of longer admissions and need to be counted as such.

  

APDC data used to prepare this code was supported by a NSW Institute of Trauma and Management (ITIM) Grant. For the details of that project see: [https://osf.io/63qc7/](https://osf.io/63qc7/)

  

**Approach used here**

  

Rather than using specific character fields describing modes of separation or transfers, the script below looks for overlapping episode periods and assumes that where an episode overlaps, it is part of the same encounter. Corrections are then applied for specific situations such as an episode starting within 12 hours of the end of another episode, or where a patient has been transferred out of NSW and transferred back in.

  

This approach is consistent with the naming conventions outlined in [Vallmuur K, McCreanor V, Cameron C, et al. Inj Prev 2021;27:479–489](https://injuryprevention.bmj.com/content/27/5/479) where an _Episode_ is the discrete unit of activity for a patient, and also referred to as a separation. Separations may include discharge, transfer or death, or ‘statistical’ separations such as episode type changes). An _Encounter_ is made up of contiguous episodes of care. This can include episodes between health services so long as they are related temporally. Episodes usually have no more than 24–48 hours between them (in other words, depending on the data you can allow 24-48 hours between episodes and include them in the same encounter.

  

**Step 1.** Convert the SAS files to R

  

*   replace numeric codes with text from data dictionaries
*   make sure times are posixct format
*   relabel columns to something more readable
*   remove duplicate entries

  

**Step 2.** NSW and non-NSW facilities

  

*   create a list of hospitals that appear in the "facility transferred from/to" but are not part of the NSW Health directory of facilities. These are then assumed to be outside of NSW. Becomes important later.

  

**Step 3.** make\_encounters function

  

This function takes each PPN and creates the encounters

  

*   creating a column named "inpatient outpatient" that uses overlapping time periods and duration of stay to estimate if the episode is an overnight stay (1+ nights), an overnight episode that was part of a longer overnight stay, a day stay episode during a longer overnight admission, or a day stay episode. This column is not used in future calculations but is useful when reviewing the data to see a possible reason why an episode may have been subsequently classified as part of a longer encounter or not.
*   create a column that displays the time interval between an episode and the previous episode, in hours (called incriment\_int)
*   create a column that returns whether an episode period overlaps with the episode period of another episode
*   create a second column that returns the smallest row number of the overlap of episode periods. This is used to join up the overlapping episodes later
*   create a "same as next column" that returns yes or no according to whether the episodes are recorded as overlapping with each other, if there are less than 12-hours between episodes, if the patient was transferred out to an out-of-state hospital or transferred back in, or if they were discharged to a palliative care unit or psychiatric hospital
*   create a column that increments the encounter-episodes based on whether the next episode is the same as the previous (no increment) or not. In the script this is inverted.
*   after encountersnare created, adjust them for episodes that overlap by are not contiguous (eg an inpatient episode that is 3-4 rows away)
*   correct encounter number so they are sequential
*   create encounter periods, episode numbers per encounter, and a unique encounter id (enctr\_id) that can be used for grouped transformations later.

  

These PPNs are then combined in a function to a dataframe rather than a very large list of individual PPNs

  

**Step 4.** Run the function

  

This is the code the runs the function. It offers the ability to run it in "chunks" of PPNs so that it can be split across R sessions if needed to speed up the processing. For example, if it is broken into 500 chucks, 1 to 100 can be run on one R session and 101 to 200 on another as so on. These can be joined later (the script to do this is not provided as it depends on where the dataframes are saved). Otherwise, all of the PPNs can be run in 500 or 1000 groups of PPNs to help save memory.

  

**Step 5.** Put the function output back into the APDC

  

The new variables created in this process are put back into the APDC. Also replaces empty fields with NAs

  

**The new fields are:**

| Variable | Notes |
| ---| --- |
| episode\_end\_dtg | created from episode start DTG + (24 \* episode days LOS ) or (+ hours day stay length of stay) |
| episode\_pd | episode start DTG %--% episode end DTG |
| inpatient\_outpatient | overnight, episode in overnight period, day stay as inpatient or day stay. Explained above |
| enctr | encounter number per PPN |
| enctr\_episode | consecutive episode number as part of each encounter |
| enctr\_id | unique code for each encountr |
| enctr\_start\_date | earliest episode start DTG in an encunter |
| enctr\_disch\_date | latest episode end DTG in an encounter |
| enctr\_pd | admsn\_date %--% disch\_date |
| days\_los | calculated from admsn\_pd (in days) |
| hours\_los | calculated from admsn\_pd (in hours) |
