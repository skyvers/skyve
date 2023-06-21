import 'package:flutter/material.dart';
import 'skyve_textField.dart';

class SkyveHTML extends StatelessWidget {
  final String label;

  const SkyveHTML({super.key, required this.label});

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    return SkyveTextField(label: label);
  }
}
