# i6zToolkit
A GUI tool for manipulating IUCLID i6z files.

## What's it all about...
The **i6z Toolkit** is a Graphical User Interface (GUI) application for handling [IUCLID 6](https://iuclid6.echa.europa.eu) archives (i6z). 

The IUCLID (International Uniform ChemicaL Information Database) software application is developed by the European Chemicals 
Agency (ECHA) in association with the Organisation for Economic Co-operation and Development (OECD). As stated on ECHA's website:

> IUCLID 6 plays a central role in the IT environments of all organisations that manage scientific data on chemicals in a regulatory context, for example under the OECD HPV, EU Biocides and EU REACH.

The **i6z Toolkit** allows users to manipulate such archives, mainly in the areas of previewing and file management/organisation. Conveniently, this is done without the need to run the IUCLID 6 application.

## Features
The below list outlines the features available:

- **Preview archives' main information** 

  View key values such as:
  
  - Filename & path
  - Archive Type (i.e., Raw Data, Dossier)
  - UUID
  - Entity Name
  - Application creator (name & version)
  - Author
  
  Note: Preview columns are sortable by clicking on their title.

- **Rename single or multiple archives**

  Ability to rename archives to:
  
  - Entity Name (i.e., `carboxin.i6z`)
  - Archive Type + Entity Name (i.e., `RAW_DATA-carboxin.i6z`)
  - UUID (i.e., `ECB5-0ba31664-6a59-4ae5-9ffa-794543dc2196.i6z`)
  
- **View `Manifest.xml` in XML Tree View**

  The `Manifest.xml` file (contained within i6z archives) hosts key information regarding the archive. The Tree View layout allows hierarchical structured access to all this data.

- **Move/Copy single or multiple archives**

  Coupled with the **Preview** feature, specific archives can be cherry-picked and then moved or copied to other storage locations for further processing.

- **Export archives' information to CSV file**

  Save to a Comma-Separated Values (CSV) file all information available in the **Preview** table.
  
  Note: The exported data retains the applied sorting order of the columns.
  
## Screenshots

Main application window:

![Screenshot-i6zToolkit-MainWindow](https://user-images.githubusercontent.com/4114200/63638932-d4420000-c696-11e9-9ef6-6e90decb8cbc.png)

XML Tree View window:

![Screenshot-i6zToolkit-XMLTreeView](https://user-images.githubusercontent.com/4114200/63638939-e4f27600-c696-11e9-910c-b4fc57fe65c1.png)

## Programming language & IDE Tool

The **i6z Toolkit** was developed with the [Free Pascal ](https://www.freepascal.org) language using [Lazarus](https://www.lazarus-ide.org), a Delphi compatible cross-platform Integrated Development Environment (IDE) for Rapid Application Development (RAD).

Note: You might need to add to the Lazarus IDE the following packages "LazControls" and "LCL". To do so, select from the menu "Project -> Project inspector...", then right-click on the "Required Packages" node and select "Add".

## Pre-built application binary

You can download a  Microsoft Windows-compatible pre-built file from [here](https://www.dropbox.com/s/5vwyddfctffdgg8/i6zToolkit-0.95-Beta_Win.zip?dl=0).

Notes:

- Just to be on the safe side, keep a backup copy of the files fed to the tool.
- Your web browser and/or Windows operating system may, incorrectly, flag the download/application as not safe. The reason for this is that the software has not been signed with a developer's certificate. Since I'm not a professional developer, and certificates cost money, the tool has been left unsigned. If you're still uncertain, you can easily scan the file with your anti-virus software. If MS Windows blocks it, then click on the "More info" option in the displayed dialog and then click on the "Run anyway" button.
- If you need a set of i6z archives to test this tool, then download these [Reference substances](https://iuclid6.echa.europa.eu/web/iuclid/get-reference-substances) from ECHA's IUCLID website.
- In theory, I should be able to provide soon pre-built versions of **i6z Toolkit** for GNU/Linux and macOS systems. So stay tuned...

## Like what you see?

If you find the **i6z Toolkit** interesting then why not buy me a coffee?

[![BuyMeACoffee](https://user-images.githubusercontent.com/4114200/63639089-672f6a00-c698-11e9-9fac-3b6fcac47901.png)](https://www.buymeacoffee.com/ADYsLjqfi)
