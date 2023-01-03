import 'package:flutter/material.dart';
import 'skyve_textField.dart';

class SkyveRadio extends StatelessWidget {
  final String label;

  const SkyveRadio({super.key, required this.label});

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    return SkyveTextField(label: label);
  }
}
