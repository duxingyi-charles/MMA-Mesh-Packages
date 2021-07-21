# MMA-Mesh-Packages
Mathematica mesh related packages

# Install
To use these packages in Mathematica, download and add the folder to Mathematica's PATH. This can be done by adding
the following line to your User Mathematica initialization file (usually located at `$UserBaseDirectory/Kernel/init.m"`. Evaluate $UserBaseDirectory in Mathematica to find out that directory on your computer.)
```
AppendTo[$Path,"/path/to/MMA-Mesh-Packages"];
```
You can find the user base directory by evaluating `$UserBaseDirectory` in Mathematica.

When all is done, you can load a package in Mathamatica, for example, loading MeshUtil package:
```
<<MeshUtil`
```
Check out the functions in MeshUtil`:
```
?MeshUtil`*
```
