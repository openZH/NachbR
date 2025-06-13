# Variable-list

In this data set, most of the variables repeat themselves for the different entities involved in a building project. For example, the project framer as well as the building contractor can have a legal entity. Therefore, all variable chunks in the below table that **end with "\*"** have to be combined in with one of the chunks that **start with "\*"**. For example, `buildingContractor*` and `*_legalEntity_selectType` becomes `buildingContractor_legenEntity_selectType` which stands for "Unterscheidung natürliche/juristische Personen bei der Bauherrschaft". If there are no stars, it is directly the name of a variable in the data set.


| Variable name                   | Amtsblattäquivalent                                                                               |
| ------------------------------- | ------------------------------------------------------------------------------------------------- |
| id                              | ID                                                                                                |
| publicationNumber               | Meldungsnummer                                                                                    |
| publicationDate                 | Veröffentlichungsdatum                                                                            |
| entryDeadline                   | Ablauf der Frist                                                                                  |
| expirationDate                  | Öffentlich einsehbar bis                                                                          |
| bfs_nr                          | BFS-Nummer                                                                                        |
| municipality_name               | Gemeindename                                                                                      |
| projectFramer_selectType        | Projektverfasser Typ                                                                              |
| projectDescription              | Projektbeschreibung                                                                               |
| buildingContractor\*            | Bauherrschaft                                                                                     |
| projectFramer\*                 | Projektverfasser                                                                                  |
| delegation_buildingContractor\* | Vertretung der Bauherrschaft                                                                      |
| projectLocation\*               | Projektstandort                                                                                   |
| districtCadastre\*              | Liegenschaftinformation                                                                           |
| \*_legalEntity_selectType       | Unterscheidung natürliche/juristische Person                                                      |
| \*_noUID                        | TRUE = keine UID in original Publikation vorhanden, FALSE = UID in original Publikation vorhanden |
| \*_index                        | Index, bei Mehrfachausprägungen eines Merkmals innerhalb einer Publikation                        |
| \*_company_legalForm            | Rechtsform-Code                                                                                   |
| \*_company_legalForm_de         | Rechtsform                                                                                        |
| \*_company_address_swissZipCode | Firmenstandort PLZ                                                                                |
| \*_company_address_town         | Firmenstandort                                                                                    |
| \*_address_index                | Index, bei mehreren Projektadressen innerhalb einer Publikation                                   |
| \*_address_street               | Strasse                                                                                           |
| \*_address_houseNumber          | Hausnummer                                                                                        |
| \*_address_swissZipCode         | PLZ                                                                                               |
| \*_address_town                 | Ortschaft                                                                                         |
| \*_relation_cadastre            | bereinigte Katasternummer                                                                         |
| \*_relation_cadastre_raw        | original Katasternummer                                                                           |
| \*_relation_buildingZone        | Zonentyp                                                                                          |
| \*_relation_district            | Kreis                                                                                             |