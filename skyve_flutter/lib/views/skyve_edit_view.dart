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
  final Map<String, String> queryParams;

  const SkyveEditView({Key? key, this.queryParams = const {}})
      : super(key: key);

  @override
  State<StatefulWidget> createState() {
    return _SkyveEditViewState();
  }

  String? get _bizId {
    return queryParams['bizId'];
  }

  String? get _bizModule {
    return queryParams['bizModule'];
  }

  String? get _bizDocument {
    return queryParams['bizDocument'];
  }
}

class _SkyveEditViewState extends State<SkyveEditView> {
  Map<String, dynamic> _bean = {'_title': 'Loading'};

  @override
  void initState() {
    super.initState();
    _load(widget._bizId);
  }

  @override
  Widget build(BuildContext context) {
    return SkyveView.responsiveView(
        context,
        '/admin/Contact',
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

  void _load(String? bizId) async {
    if (_bean['bizId'] == null) {
      final bean = await SkyveRestClient().edit('admin', 'Contact', bizId);
      setState(() {
        _bean = bean;
      });
    }
  }
}
