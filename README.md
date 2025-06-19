# Reusable-DT

This repository contains code used for the MSc Thesis: Towards Digital Twin Reusability in
Virtual Research Environments: A Honeybee-pDT Case Study.

The actual code was implemented in the NaaVRE environment. This means that it is not possible to run this code in it's current state outside of the NaaVRE environment. It also means that the code and files here are not the same as the ones used in the NaaVRE environment. The code here is a draft of the components used in the NaaVRE environment. Further details are in the thesis.

Datafiles are not included in this repository and should be requested from the authors of the original Honeybee-pDT project. The files required can be seen in the config file in the `components` folder.

The code is organized into three main folders:
- `components`: Contains the reusable problem solving components used in the Digital Twin (DT) and the DT itself.
- `draft`: Contains the draft of the components as a combined component.
- `netlogo-r flavor`: contains the files used to build a Docker image that can run the DT in a Netlogo environment in NaaVRE. Not standalone, but part of the https://github.com/QCDIS/NaaVRE-flavors repository.

Further repositories used in the project:
- [BioDT Shiny](https://github.com/BioDT/biodt-shiny/tree/main?tab=readme-ov-file)
- [BioDT UC Pollinators Container](https://github.com/BioDT/uc-pollinators-container?tab=readme-ov-file)
- [BioDT UC Pollinators Beehave](https://github.com/BioDT/uc-pollinators-beehave)
- [BioDT UC Pollinators](https://github.com/BioDT/uc-pollinators)
- [QCDIS NaaVRE](https://github.com/QCDIS/NaaVRE)
- [QCDIS NaaVRE Flavors](https://github.com/QCDIS/NaaVRE-flavors)
- [QCDIS NaaVRE Dev Environment](https://github.com/QCDIS/NaaVRE-dev-environment)