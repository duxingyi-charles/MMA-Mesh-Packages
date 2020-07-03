# MMA-Mesh-Packages
Mathematica mesh related packages

# Install
To use this package in Mathematica, download and add the folder to Mathematica's PATH. You can add the following line to your User Mathematica initialization file (usually located at `$UserBaseDirectory/Kernel/init.m"`)
```
AppendTo[$Path,"/Users/charlesdu/MMA-Mesh-Packages"];
```
You can find this Applications folder by evaluating `$UserBaseDirectory` in Mathematica and looking for the pre-existing Applications folder under that directory.

When all is done, you can load a package in Mathamatica, for example, loading MeshUtil package:
```
<<MeshUtil`
```
Check out the functions in MeshUtil`:
```
?MeshUtil`*
```
