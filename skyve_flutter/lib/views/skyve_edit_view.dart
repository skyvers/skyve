import 'package:flutter/material.dart';
import '../util/skyve_rest_client.dart';
import '../widgets/skyve_view.dart';
import '../widgets/skyve_border.dart';
import '../widgets/skyve_button.dart';
import '../widgets/skyve_combo.dart';
import '../widgets/skyve_contentimage.dart';
import '../widgets/skyve_form.dart';
import '../widgets/skyve_formitem.dart';
import '../widgets/skyve_formrow.dart';
import '../widgets/skyve_hbox.dart';
import '../widgets/skyve_label.dart';
import '../widgets/skyve_textfield.dart';
import '../widgets/skyve_toolbar.dart';

class SkyveEditView extends StatefulWidget {
  final String m;
  final String d;
  final String? i;

  const SkyveEditView({Key? key, required this.m, required this.d, this.i})
      : super(key: key);

  @override
  State<StatefulWidget> createState() {
    return _SkyveEditViewState();
  }
}

class _SkyveEditViewState extends State<SkyveEditView> {
  Map<String, dynamic> _bean = {'_title': 'Loading'};

  @override
  void initState() {
    super.initState();
    _load(widget.m, widget.d, widget.i);
  }

  @override
  Widget build(BuildContext context) {
    return SkyveView.responsiveView(
        context,
        _bean['_title'],
        Visibility(
            visible: (_bean['bizId'] != null),
            replacement: const Center(child: CircularProgressIndicator()),
            child: SingleChildScrollView(
              child: Column(children: [
                SkyveToolbar(children: [
                  Container(
                      padding: const EdgeInsets.symmetric(horizontal: 10.0),
                      child: Wrap(
                          alignment: WrapAlignment.center,
                          spacing: 8.0,
                          children: const [
                            SkyveButton(name: 'OK', label: 'OK'),
                            SkyveButton(name: 'Save', label: 'Save'),
                            SkyveButton(name: 'Delete', label: 'Delete'),
                            SkyveButton(name: 'ZoomOut', label: 'Zoom Out'),
                            SkyveButton(name: 'Cancel', label: 'Cancel'),
                            SkyveButton(name: 'Remove', label: 'Remove'),
                          ])),
                ]),
                SkyveBorder(
                  child: SkyveHBox(children: [
                    SkyveForm(formCols: const [], formRows: [
                      SkyveFormRow(formItems: [
                        const SkyveFormItem(SkyveLabel('Name')),
                        SkyveFormItem(
                          SkyveTextField(
                              label: 'Name', initialValue: nvl(_bean['name'])),
                        ),
                      ]),
                      const SkyveFormRow(formItems: [
                        SkyveFormItem(SkyveLabel('Contact Type')),
                        SkyveFormItem(
                          SkyveCombo(label: 'Contact Type'),
                        ),
                      ]),
                      SkyveFormRow(formItems: [
                        const SkyveFormItem(SkyveLabel('Email')),
                        SkyveFormItem(
                          SkyveTextField(
                              label: 'Email',
                              initialValue: nvl(_bean['email1'])),
                        ),
                      ]),
                      SkyveFormRow(formItems: [
                        const SkyveFormItem(SkyveLabel('Mobile')),
                        SkyveFormItem(
                          SkyveTextField(
                              label: 'Mobile',
                              initialValue: nvl(_bean['mobile'])),
                        ),
                      ]),
                    ]),
                    const SkyveForm(formCols: [], formRows: [
                      SkyveFormRow(formItems: [
                        SkyveFormItem(SkyveLabel('Image')),
                        SkyveFormItem(
                          SkyveContentImage(label: 'Image'),
                        ),
                      ]),
                    ]),
                  ]),
                ),
              ]),
            )));
  }

  void _load(String m, String d, String? i) async {
    if (_bean['bizId'] == null) {
      final bean = await SkyveRestClient().edit(m, d, i);
      setState(() {
        _bean = bean;
      });
    }
  }
}
