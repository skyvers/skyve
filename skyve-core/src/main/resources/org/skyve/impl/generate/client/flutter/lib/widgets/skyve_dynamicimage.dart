import 'package:flutter/material.dart';
import 'skyve_textField.dart';

class SkyveDynamicImage extends StatelessWidget {
  final String label;

  const SkyveDynamicImage({super.key, required this.label});

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    return SkyveTextField(label: label);
  }
}
