import 'package:flutter/material.dart';
import 'skyve_textfield.dart';

class SkyveUpload extends StatelessWidget {
  final String label;

  const SkyveUpload({super.key, required this.label});

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    return SkyveTextField(label: label);
  }
}
