Details
-------

This application was developed by [Charlotte M.
Jones-Todd](mailto:cmjonestodd@gmail.com) for the IUCN SSC primate
specialist group section on small apes. This application uses the `R`
package [ascr](https://github.com/b-steve/ascr) developed by [Ben C.
Stevenson](http://bcstevenson.nfshost.com).

All source code for the application is available
[here](https://github.com/cmjt/ascr/tree/master/inst/shiny-examples/ascr)

#### **A manual which details using this user interface to fit spatial capture-recapture models for acoustic data is available**

#### **[here as a pdf](https://github.com/cmjt/ascr/blob/master/docs/index.pdf), or**

#### **[here as a webpage](https://cmjt.github.io/ascr/).**

Overview
--------

This user interface works in conjunction with the
[ascr](https://github.com/b-steve/ascr) package providing software
implementation for the fitting of spatial capture-recapture (SCR) models
for acoustic data.

All operations are carried out by the user in the left hand sidebar.
Output is displayed in one of the **Data**, **Mask**, or **Model** tabs.

Either load your own data or use example data included with the
software. The **trap location** file must contain columns named **x**,
**y**, and **post** identifying UTM coordinates and identifier of each
trap (currently only a single array of traps is supported). The
**detections** file must contain columns named **occasion**, **group**,
and **post** that correspond to the occasion at which each group was
heard by which trap (corresponding to the traps in the **trap location**
file). The **detections** file may also contain a column headed
**bearing** or **distance** or both. These must refer to the estimated
bearing of the call (radians ∈(0, 2*π*)) and the estimated distance to
the call (meters). If this is the case then additional parameters are
estimated related to these distributions.

Select the **Data** tab to visualize your data (both trap locations and
detections).

Under the **Build mask** section the mask (a required component of the
SCR modelling framework) **buffer** and **spacing** may be set.

Select the **Mask** tab to visualize the mask.

Under the **Modelling** section different detection functions may be
chosen as well as the ability to fix certain parameters to a particular
value (i.e., not estimated during modelling procedure but set to some
fixed value). The **Fit model** button initiates the model fitting
procedure

Select the **Model** tab to visualize the model outputs.

Advanced options such as giving parameter starting values or saving
model as an `R` object are available when **Show advanced options** is
chosen.

Use the **Download** buttons to save plots or model data. In addition,
create a basic report which contains an animation of estimated locations
based on detections **Generate Basic Report**.
