import 'package:flutter/material.dart';
import '../widgets/skyve_blurb.dart';
import '../widgets/skyve_button.dart';
import '../widgets/skyve_contentimage.dart';
import '../widgets/skyve_form.dart';
import '../widgets/skyve_formitem.dart';
import '../widgets/skyve_formrow.dart';
import '../widgets/skyve_hbox.dart';
import '../widgets/skyve_label.dart';
import '../widgets/skyve_password.dart';
import '../widgets/skyve_textfield.dart';
import '../widgets/skyve_vbox.dart';
import '../widgets/skyve_view.dart';

class SkyveViewModel implements SkyveView {
  final String module;
  final String document;
  final Map<String, dynamic> json;

  const SkyveViewModel(
      {required this.module, required this.document, required this.json});

  @override
  List<Widget> actions(BuildContext context, Map<String, dynamic> bean) {
    List<dynamic>? actions = json['actions'];
    final List<Widget> result = [];
    if (actions != null) {
      for (Map<String, dynamic> action in actions) {
        final bool inActionPanel = action['inActionPanel'];
        if (inActionPanel) {
          final String type = action['type'];
          final String actionType = action['actionType'];
          final bool clientValidation = action['clientValidation'];
          final String? fontIcon = action['fontIcon'];
          final String? iconUrl = action['iconUrl'];
          final String label = action['label'];
          final String name = action['name'];
          final String show = action['show'];
          final String? resourceName = action['resourceName'];
          result.add(SkyveButton(type: actionType, name: name, label: label));
        }
      }
    }
    return result;
  }

  @override
  List<Widget> contained(BuildContext context, Map<String, dynamic> bean) {
    return _many(context, json['contained'], bean);
  }

  static List<Widget> _many(BuildContext context, List<dynamic> contained,
      Map<String, dynamic> bean) {
    final List<Widget> result = List.generate(contained.length, (index) {
      Map<String, dynamic> model = contained[index];
      return _one(context, model, bean, null);
    }, growable: false);
    return result;
  }

  static Widget _one(BuildContext context, Map<String, dynamic> model,
      Map<String, dynamic> bean, String? formLabel) {
    final String type = model['type'];
    switch (type) {
      case 'actionLink':
        return const Text('actionLink');
      case 'actionRef':
        return const Text('actionRef');
      case 'addAction':
        return const Text('addAction');
      case 'blurb':
        return SkyveBlurb(label: model['markup']);
      case 'boundColumn':
        return const Text('boundColumn');
      case 'border':
        return const Text('border');
      case 'button':
        return const Text('border');
      case 'cancelAction':
        return const Text('cancelAction');
      case 'chart':
        return const Text('chart');
      case 'checkBox':
        return const Text('checkBox');
      case 'checkMembership':
        return const Text('checkMembership');
      case 'colour':
        return const Text('colour');
      case 'column':
        return const Text('column');
      case 'combo':
        return const Text('combo');
      case 'comparison':
        return const Text('comparison');
      case 'containerColumn':
        return const Text('containerColumn');
      case 'contentColumn':
        return const Text('contentColumn');
      case 'contentImage':
        return SkyveContentImage(label: formLabel ?? '');
      case 'contentLink':
        return const Text('contentLink');
      case 'contentRef':
        return const Text('contentRef');
      case 'contentSignature':
        return const Text('contentSignature');
      case 'dataGrid':
        return const Text('dataGrid');
      case 'dataRepeater':
        return const Text('dataRepeater');
      case 'deleteAction':
        return const Text('deleteAction');
      case 'dialogButton':
        return const Text('dialogButton');
      case 'download':
        return const Text('download');
      case 'downloadAction':
        return const Text('downloadAction');
      case 'dyanmicImage':
        return const Text('dyanmicImage');
      case 'editAction':
        return const Text('editAction');
      case 'editViewRef':
        return const Text('editViewRef');
      case 'exportAction':
        return const Text('exportAction');
      case 'externalRef':
        return const Text('externalRef');
      case 'form':
        return SkyveForm(
            formCols: [], formRows: _formRows(context, model['rows'], bean));
      case 'implicitActionRef':
        return const Text('implicitActionRef');
      case 'importAction':
        return const Text('importAction');
      case 'item':
        return const Text('item');
      case 'geometry':
        return const Text('geometry');
      case 'geometryMap':
        return const Text('geometryMap');
      case 'hbox':
        return SkyveHBox(children: _many(context, model['contained'], bean));
      case 'html':
        return const Text('html');
      case 'inject':
        return const Text('inject');
      case 'link':
        return const Text('link');
      case 'listGrid':
        return const Text('listGrid');
      case 'listRepeater':
        return const Text('listRepeater');
      case 'listViewRef':
        return const Text('listViewRef');
      case 'label':
        return const Text('label');
      case 'listMembership':
        return const Text('listMembership');
      case 'lookupDescription':
        return const Text('lookupDescription');
      case 'map':
        return const Text('map');
      case 'navigateAction':
        return const Text('navigateAction');
      case 'newAction':
        return const Text('newAction');
      case 'okAction':
        return const Text('okAction');
      case 'password':
        return SkyvePassword(label: formLabel ?? '');
      case 'printAction':
        return const Text('printAction');
      case 'progressBar':
        return const Text('progressBar');
      case 'queryListViewRef':
        return const Text('queryListViewRef');
      case 'radio':
        return const Text('radio');
      case 'report':
        return const Text('report');
      case 'reportAction':
        return const Text('reportAction');
      case 'reportRef':
        return const Text('reportRef');
      case 'removeAction':
        return const Text('removeAction');
      case 'rerender':
        return const Text('rerender');
      case 'resourceRef':
        return const Text('resourceRef');
      case 'richText':
        return const Text('richText');
      case 'row':
        return const Text('row');
      case 'saveAction':
        return const Text('saveAction');
      case 'server':
        return const Text('server');
      case 'serverAction':
        return const Text('serverAction');
      case 'setDisabled':
        return const Text('setDisabled');
      case 'setInvisible':
        return const Text('setInvisible');
      case 'slider':
        return const Text('slider');
      case 'spacer':
        return const Text('spacer');
      case 'spinner':
        return const Text('spinner');
      case 'staticImage':
        return const Text('staticImage');
      case 'tab':
        return const Text('tab');
      case 'tabPane':
        return const Text('tabPane');
      case 'textArea':
        return const Text('textArea');
      case 'textField':
        return SkyveTextField(
            label: formLabel ?? '', initialValue: nvl(bean[model['binding']]));
      case 'toggleDisabled':
        return const Text('toggleDisabled');
      case 'toggleVisibility':
        return const Text('toggleVisibility');
      case 'treeGrid':
        return const Text('treeGrid');
      case 'upload':
        return const Text('upload');
      case 'uploadAction':
        return const Text('uploadAction');
      case 'vbox':
        return SkyveVBox(children: _many(context, model['contained'], bean));
      case 'zoomIn':
        return const Text('zoomIn');
      case 'zoomOutAction':
        return const Text('zoomOutAction');
      default:
        return Text('unsupported $type');
    }
  }

  static List<SkyveFormRow> _formRows(
      BuildContext context, List<dynamic> rows, Map<String, dynamic> bean) {
    List<SkyveFormRow> result = List.generate(rows.length, (index) {
      Map<String, dynamic> row = rows[index];
      return SkyveFormRow(formItems: _formItems(context, row['items'], bean));
    }, growable: false);
    return result;
  }

  static List<SkyveFormItem> _formItems(
      BuildContext context, List<dynamic> items, Map<String, dynamic> bean) {
    List<SkyveFormItem> result = [];
    for (Map<String, dynamic> item in items) {
      final String? label = item['label'];
      final bool showsLabel = item['showsLabel'];
      final bool required = item['required'];
      if ((showsLabel) && (label != null)) {
        result.add(SkyveFormItem(SkyveLabel(label)));
      }
      result.add(SkyveFormItem(
          _one(context, item['widget'], bean, (showsLabel ? label : null))));
    }
    return result;
  }
}
