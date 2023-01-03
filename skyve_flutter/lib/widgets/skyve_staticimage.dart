import 'package:flutter/material.dart';
import 'skyve_textField.dart';

class SkyveStaticImage extends StatelessWidget {
  final String label;

  const SkyveStaticImage({super.key, required this.label});

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    return SkyveTextField(label: label);
  }
}
