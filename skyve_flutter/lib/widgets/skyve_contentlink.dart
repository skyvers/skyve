import 'package:flutter/material.dart';
import 'skyve_textField.dart';

class SkyveContentLink extends StatelessWidget {
  final String label;

  const SkyveContentLink({super.key, required this.label});

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    return SkyveTextField(label: label);
  }
}
