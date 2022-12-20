import 'package:##PROJECT##/widgets/skyve_textField.dart';
import 'package:flutter/material.dart';

class SkyveColourPicker extends StatelessWidget {
  final String label;

  const SkyveColourPicker({super.key, required this.label});

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    return SkyveTextField(label: label);
  }
}
