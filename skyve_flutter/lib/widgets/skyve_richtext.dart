import 'package:flutter/material.dart';
import 'skyve_textfield.dart';

class SkyveRichText extends StatelessWidget {
  final String label;

  const SkyveRichText({super.key, required this.label});

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    return SkyveTextField(label: label);
  }
}
