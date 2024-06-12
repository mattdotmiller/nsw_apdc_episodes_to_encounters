# Grouping Episodes of Care into Patient Encounters for the New South Wales Admitted Patient Data Collection (APDC)

_Matthew Miller_

_With advice from Blanca Gallego, Louisa Jorm_ and _Dami Sotade_

updated 22 Aug 23

  

**APDC data used to prepare this code was supported by a NSW Institute of Trauma and Management (ITIM) Grant**

  

_For the details of that project see:_ [https://osf.io/63qc7/](https://osf.io/63qc7/)

For the accompanying journal article that used this script see: [https://doi.org/10.1016/j.injury.2024.111570](https://doi.org/10.1016/j.injury.2024.111570)

  

**Overview**

_The admitted patient data collection (APDC) contains patient care episodes rather than admissions or encounters. An encounter needs to be created by combining the appropriate patient care episodes. There are a number of fields that could be helpful in determining when the end of a patient care episode corresponds to the end of an encounter, for example 'mode of separation' is recorded as "Discharged by Hospital". Unfortunately, the documentation of a "Discharge" is not always helpful. For example, patients can be transferred between hospitals but are recorded as "Discharged from Hospital" on transfer although their inpatient episode continues at a second hospital. Patients on renal dialysis are always documented as being discharged from hospital at the end of their dialysis treatment, even if they are an inpatient. In addition, some multiple-day episodes are nested parts of longer encounters and need to be counted as such._

  

  

**Approach used here**

  

Rather than using specific character fields describing modes of separation or transfers, the script below looks for overlapping episode periods and assumes that where an episode overlaps, it is part of the same encounter. Corrections are then applied for specific situations such as an episode starting within 12 hours of the end of another episode, or where a patient has been transferred out of NSW and transferred back in.

  

This approach is consistent with the naming conventions outlined in [Vallmuur K, McCreanor V, Cameron C, et al. Inj Prev 2021;27:479–489](https://injuryprevention.bmj.com/content/27/5/479) where an _Episode_ is the discrete unit of activity for a patient, and also referred to as a separation. Separations may include discharge, transfer or death, or ‘statistical’ separations such as episode type changes. An _Encounter_ is made up of contiguous episodes of care. This can include episodes between health services so long as they are related temporally. Episodes usually have no more than 24–48 hours between them (in other words, depending on the data you can allow 24-48 hours between episodes and include them in the same encounter). In the script 12 hours is used, but this can be adjusted to your preference.

  

  

**Step 1.** Convert the SAS files to R

  

*   replace numeric codes with text from data dictionaries
*   make sure times are posixct format
*   relabel columns to something more readable
*   remove duplicate entries

  

**Step 2.** NSW and non-NSW facilities

  

*   create a list of hospitals that appear in the "facility transferred from/to" but are not part of the NSW Health directory of facilities. These are then assumed to be outside of NSW. Becomes important later.

  

**Step 3.** make\_encounters function

  

This function takes each PPN and creates the encounters

  

Line 280 lets you set the time window for which two sequential episodes can be considered part of the same encounter.

  

*   create a column that displays the time interval between an episode and the previous episode, in hours (called incriment\_int)
*   create a column that returns whether an episode period overlaps with the episode period of another episode
*   create a second column that returns the smallest row number of the overlap of episode periods. This is used to join up the overlapping episodes later
*   create a "same as next column" that returns yes or no according to whether the episodes are recorded as overlapping with each other, if there are less than 12-hours between episodes, if the patient was transferred out to an out-of-state hospital or transferred back in, or if they were discharged to a palliative care unit or psychiatric hospital
*   create a column that increments the encounter-episodes based on whether the next episode is the same as the previous (no increment) or not. In the script this is inverted.
*   after encounters are created, adjust them for episodes that overlap but are not contiguous (eg an inpatient episode that is 3-4 rows away)
*   correct encounter number so they are sequential
*   create encounter periods, episode numbers per encounter, and a unique encounter id (enctr\_id) that can be used for grouped transformations later.

These PPNs are then combined in a function to a dataframe rather than a very large list of individual PPNs

  

  

**Step 4.** Run the function

  

This is the code the runs the function. It offers the ability to run it in "chunks" of PPNs so that it can be split across R sessions if needed to speed up the processing. For example, if it is broken into 500 chucks, 1 to 100 can be run on one R session and 101 to 200 on another as so on. These can be joined later (the script to do this is not provided as it depends on where the dataframes are saved). Otherwise, all of the PPNs can be run in 500 or 1000 groups of PPNs to help save memory.

  

**Step 5.** Put the function output back into the APDC

  

The new variables created in this process are put back into the APDC. Also replaces empty fields with NAs

  

The new fields are:

| Variable | Notes |
| ---| --- |
| episode\_end\_dtg | created from episode start DTG + (24 \* episode days LOS ) or (+ hours day stay length of stay) |
| episode\_pd | episode start DTG %--% episode end DTG |
| inpatient\_outpatient | overnight, episode in overnight period, day stay as inpatient or day stay. Explained above |
| enctr | encounter number per PPN |
| enctr\_episode | consecutive episode number as part of each encounter |
| enctr\_id | unique code for each encounter |
| enctr\_start\_date | earliest episode start DTG in an encounter |
| enctr\_disch\_date | latest episode end DTG in an encounter |
| enctr\_pd | admsn\_date %--% disch\_date |
| days\_los | calculated from admsn\_pd (in days) |
| hours\_los | calculated from admsn\_pd (in hours) |

# Collapsing re-admissions for injured patients

  

For injured patients, the APDC dataset may contain an initial episodes that are then grouped into encounters. Subsequent episodes may include re-admissions for the same injury (for example a patient who is discharged following a hip fracture due to a fall, and orthopedic surgery for the same, but is readmitted 2 weeks later for a post-operative infection). These may be counted as two separate encounters, or can be collapsed into a single encounter with time separated episodes.

  

**Step 6** of the script collapses encounters where a readmission for the same principle diagnosis, or same mechanisms of injury using ICD-10-AM codes, set to a maximum of 28-days between encounters (see [Falsteret al. Disentangling the impacts of geography and Aboriginality on serious road transport injuries in New South Wales. Accid Anal Prev 54 (2013) 32–8](http://doi.org/10.1016/j.aap.2013.01.015) for why this period chosen).

  

The script also leaves intervening encounters not meeting the criteria above as separate. For example if a patient is 1. admitted and discharged for a hip fracture post fall, 2. admitted 7 days later for cataract surgery, then 3. admitted 20 days after discharge from the hip fracture initial admission with a post-operative hip infection, only encounters 1 and 3 will be combined.

  

The new fields are:

| Variable | Notes |
| ---| --- |
| enctr\_id\_c | unique code for each encounter, recycles the first enctr\_id from the APDC dataframe, so uncollapse encounters should have their original enctr\_id |
| enctr\_start\_date\_c | earliest enctr\_start\_date |
| enctr\_disch\_date\_c | final enctr\_end\_date from the last encounter |
| enctr\_pd\_c | enctr\_start\_date\_c %--% enctr\_disch\_date\_c |
| enctr\_days\_los | sums the days\_los for all episodes. Will be shorter than the enctr\_pd\_c which will include time out of hospital. |
| enctr\_hrs\_los | sums the hrs\_los for all episodes. Will be shorter than the enctr\_pd\_c which will include time out of hospital. |

These fields are then added to the "apdc" dataframe to make a new "apdc\_trauma" dataframe.
