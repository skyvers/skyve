import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/skyve_enums.dart';

class SkyveFormItem extends StatelessWidget {
  final Widget item;
  final HorizontalAlignment? align;
  final int colspan;
  final String? help;
  final String? label;
  final HorizontalAlignment? labelAlign;
  final bool required;
  final int rowspan;
  final bool showHelp;
  final bool showLabel;

  const SkyveFormItem(this.item,
      {super.key,
      this.align,
      this.colspan = 1,
      this.help,
      this.label,
      this.labelAlign,
      this.required = false,
      this.rowspan = 1,
      this.showHelp = true,
      this.showLabel = true});

  @override
  Widget build(BuildContext context) {
    return item;
  }
}
