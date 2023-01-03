import 'package:flutter/material.dart';
import 'skyve_textField.dart';

class SkyveContentSignature extends StatelessWidget {
  final String label;

  const SkyveContentSignature({super.key, required this.label});

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    return SkyveTextField(label: label);
  }
}
