import 'package:##PROJECT##/widgets/skyve_textField.dart';
import 'package:flutter/material.dart';

class SkyveTextArea extends StatelessWidget {
  final String label;

  const SkyveTextArea({super.key, required this.label});

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    return SkyveTextField(label: label, maxlines: null);
  }
}
