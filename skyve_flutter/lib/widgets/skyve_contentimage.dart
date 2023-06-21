import 'package:flutter/material.dart';
import 'skyve_textField.dart';

class SkyveContentImage extends StatelessWidget {
  final String label;

  const SkyveContentImage({super.key, required this.label});

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    return SkyveTextField(label: label);
  }
}
