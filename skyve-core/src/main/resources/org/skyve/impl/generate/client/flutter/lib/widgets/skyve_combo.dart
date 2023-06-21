import 'package:flutter/material.dart';
import 'skyve_textField.dart';

class SkyveCombo extends StatelessWidget {
  final String label;

  const SkyveCombo({super.key, required this.label});

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    return SkyveTextField(label: label);
  }
}
