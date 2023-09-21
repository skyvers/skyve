import 'package:flutter/material.dart';

class SkyveBlurb extends StatelessWidget {
  final String label;

  const SkyveBlurb({super.key, required this.label});

  @override
  Widget build(BuildContext context) {
    return const Placeholder(
      fallbackHeight: 50,
      color: Colors.orange,
    );
  }
}
