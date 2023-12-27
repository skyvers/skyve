import 'package:flutter/material.dart';

class SkyveCheckBox extends StatefulWidget {
  final String label;
  final bool? isChecked;
  final bool tristate;

  const SkyveCheckBox(
      {Key? key, required this.label, required this.tristate, this.isChecked})
      : super(key: key);

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
        value: checked,
        tristate: widget.tristate,
        onChanged: (value) {
          setState(() {
            checked = value;
          });
        });
  }
}
