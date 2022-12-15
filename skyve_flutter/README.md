# skyve_flutter

A project to test out some of the featurs of flutter including:

* JSON handling
* REST request creation and response handling
* Creating a custom DataTable via composition that has filtering (the standard Flutter widget does not have filtering)

## Getting Started

A few resources to get you started with Flutter:

- [Lab: Write your first Flutter app](https://flutter.dev/docs/get-started/codelab)
- [Cookbook: Useful Flutter samples](https://flutter.dev/docs/cookbook)
- [online documentation](https://flutter.dev/docs) - which offers tutorials,
samples, guidance on mobile development, and a full API reference.


## Flutter Command Line Usage

### Getting Help

```bash
flutter --help
```

### Creating a Project

This project was created using the following:

```bash
$ flutter create skyve_flutter --template app --platforms ios,android,web -a java --org org.skyve
```

* --platforms nominates the target devices for the application
* -a nominates to use the Java  language for Android instead of the default which is Kotlin

### List Available Emulators

On my machine, the following lists the available Android emulators:

```bash
flutter emulators
```

### Build the Application

```bash
flutter build web
```

### Running the Application

To run on Chrome:
```bash
flutter run -d chrome
```

To run in Android (there must be an already created emulator):
```bash
flutter run -d android
```

### CORS issues running in a browser

To run inside chrome and connect to your localhost skyve server without CORS issues use the following project...

https://pub.dev/packages/flutter_cors

```bash
dart pub global activate flutter_cors
fluttercors --disable
fluttercors --enable
```

## Flutter in VS Code

### Getting Setup

You will need to install some extensions. These might be useful:

* Flutter (possibly installs the Dart extension as well)
* GitLens
* Git History
* Ultimate Flutter Extension Pack (installs some of the above)

### Device Selection

The target device can be selected in the bottom right of the status bar. Only the devices supported by the project will be displayed in the device list. The device list is displayed in the same area as when entering commands via CTRL+SHFT+P (hint: look upwards not at the status bar).

If Android had been configured as one of the platforms for the project (which it has for this project) then clicking on the devices in the status bar will show available Android emulators or give the option to create an emulator.

### Running the Application

Hit the F5 key - if certain files have focus hitting F5 won't work so make sure that either a dart file or the file explorer has focus.

### General Commands

Hit CTRL+SHFT+P and type flutter to see what is available in VS Code.

