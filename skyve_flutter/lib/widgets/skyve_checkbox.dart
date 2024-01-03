import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/skyve_mixins.dart';

class SkyveCheckBox extends StatefulWidget with Sizable {
  final String label;
  final bool? isChecked;
  final bool tristate;

  SkyveCheckBox(
      {Key? key,
      required this.label,
      required this.tristate,
      this.isChecked,
      int? pixelWidth,
      int? pixelHeight})
      : super(key: key) {
    this.pixelWidth = pixelWidth;
    this.pixelHeight = pixelHeight;
  }

  @override
  State<StatefulWidget> createState() => _SkyveCheckBoxState();
}

class _SkyveCheckBoxState extends State<SkyveCheckBox> {
  late bool? checked;

  @override
  void initState() {
    super.initState();
    checked = widget.isChecked;
  }

  @override
  Widget build(BuildContext context) {
    return CheckboxListTile(
        title: Text(widget.label),
        // NB value can't be null if widget is not tristate
        value: (widget.tristate) ? checked : checked ?? false,
        tristate: widget.tristate,
        onChanged: (value) {
          setState(() {
            checked = value;
          });
        });
  }
}
