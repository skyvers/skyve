import 'package:flutter/material.dart';
import 'skyve_textField.dart';

class SkyveSpinner extends StatelessWidget {
  final String label;

  const SkyveSpinner({super.key, required this.label});

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    return SkyveTextField(label: label);
  }
}
