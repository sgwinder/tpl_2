## Trust for Public Lands (TPL) Workflow

This repository houses code and data for the TPL showcase project working on public lands within a 2 hour drive of Salem, OR.

`1_Preparing_polygons.r` creates a gridded version of the AOI. It pulls in an AOI shapefile, creates a grid covering the extent, and dissolves and intersects the AOI polygons with the grid. Its product is a shapefile which is ready to be fed into the `globalrec` workflow.

The folders contain data specific to this 2 hr drive from Salem AOI.
