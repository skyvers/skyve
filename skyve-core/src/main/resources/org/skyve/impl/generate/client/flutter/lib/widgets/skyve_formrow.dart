import 'package:flutter/material.dart';
import 'package:flutter_bootstrap/flutter_bootstrap.dart';
import 'skyve_formitem.dart';

class SkyveFormRow extends StatelessWidget {
  // todo bring in extra state from metadata
  // e.g. label, required
  final List<SkyveFormItem> formItems;

  const SkyveFormRow({super.key, required this.formItems});

  @override
  Widget build(BuildContext context) {
    return BootstrapContainer(children: formItems);
  }
}
