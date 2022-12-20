import 'package:##PROJECT##/widgets/skyve_textField.dart';
import 'package:flutter/material.dart';

class SkyveDownload extends StatelessWidget {
  final String label;

  const SkyveDownload({super.key, required this.label});

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    return SkyveTextField(label: label);
  }
}
