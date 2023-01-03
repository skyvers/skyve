import 'package:flutter/material.dart';
import 'skyve_textField.dart';

class SkyveDownload extends StatelessWidget {
  final String label;

  const SkyveDownload({super.key, required this.label});

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    return SkyveTextField(label: label);
  }
}
