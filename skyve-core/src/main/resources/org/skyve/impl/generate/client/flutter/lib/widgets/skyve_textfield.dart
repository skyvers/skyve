import 'package:flutter/material.dart';

class SkyveTextField extends StatefulWidget {
  final String label;
  final String? initialValue;

  const SkyveTextField({Key? key, required this.label, this.initialValue})
      : super(key: key);

  @override
  State<StatefulWidget> createState() => SkyveTextFieldState();
}

class SkyveTextFieldState extends State<SkyveTextField> {
  @override
  Widget build(BuildContext context) {
    return TextFormField(
        decoration: InputDecoration(
            border: const OutlineInputBorder(), labelText: widget.label),
        initialValue: widget.initialValue);
  }
}
