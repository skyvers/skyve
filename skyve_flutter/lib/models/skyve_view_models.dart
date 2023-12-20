import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/validators.dart';
import 'package:skyve_flutter/widgets/skyve_tab.dart';
import 'package:skyve_flutter/widgets/skyve_tabpane.dart';
import '../widgets/skyve_blurb.dart';
import '../widgets/skyve_button.dart';
import '../widgets/skyve_contentimage.dart';
import '../widgets/responsive_layout.dart';
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
  final Map<String, dynamic> jsonMetaData;

  const SkyveViewModel(
      {required this.module,
      required this.document,
      required this.jsonMetaData});

  @override
  List<Widget> actions(BuildContext context) {
    List<dynamic>? actions = jsonMetaData['actions'];
    final List<Widget> result = [];
    if (actions != null) {
      for (Map<String, dynamic> action in actions) {
        final bool inActionPanel = action['inActionPanel'];
        if (inActionPanel) {
          result.add(_createButton(action));
        }
      }
    }
    return result;
  }

  static SkyveButton _createButton(Map<String, dynamic> actionJson) {
    final String actionType = actionJson['actionType'] ?? actionJson['type'];
    final String actionName = actionJson['actionName'];
    final String label = actionJson['label'];
    final bool clientValidation = actionJson['clientValidation'] ?? false;

    return SkyveButton(
      actionType: actionType,
      actionName: actionName,
      label: label,
      clientValidation: clientValidation,
    );
  }

  @override
  List<Widget> contained(BuildContext context) {
    return _many(jsonMetaData['contained']);
  }

  static List<Widget> _many(List<dynamic> contained) {
    return contained.map((elem) => _one(model: elem)).toList();
  }

  static Widget _one(
      {required Map<String, dynamic> model,
      String? formLabel,
      bool required = false}) {
    final String type = model['type'];

    final Validator validator = _createValidators(model, formLabel, required);

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
        return _createButton(model);
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
        return SkyveContentImage(
          propertyKey: model['binding'],
          label: formLabel ?? '',
        );
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
        {
          List<SkyveFormRow> formRows = _formRows(model['rows']);
          return ResponsiveLayout(formCols: const [], formRows: formRows);
        }
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
        return SkyveHBox(children: _many(model['contained']));
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
        return SkyvePassword(
          propertyKey: model['binding'],
          label: formLabel ?? '',
          validator: validator.validate,
        );
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
        return SkyveTab(
            title: model['title'],
            icon: model['icon'],
            children: _many(model['contained']));
      case 'tabPane':
        return SkyveTabPane(tabs: _tabs(model['tabs']));
      case 'textArea':
        return const Text('textArea');
      case 'textField':
        return SkyveTextField(
          propertyKey: model['binding'],
          label: formLabel ?? '',
          validator: validator.validate,
        );
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
        return SkyveVBox(
            children: _many(
          model['contained'],
        ));
      case 'zoomIn':
        return const Text('zoomIn');
      case 'zoomOutAction':
        return const Text('zoomOutAction');
      default:
        return Text('unsupported $type');
    }
  }

  static List<SkyveFormRow> _formRows(List<dynamic> rows) {
    return rows
        .map((row) => SkyveFormRow(formItems: _formItems(row['items'])))
        .toList();
  }

  static List<SkyveFormItem> _formItems(List<dynamic> items) {
    List<SkyveFormItem> result = [];
    for (Map<String, dynamic> item in items) {
      final String? label = item['label'];
      final bool showsLabel = item['showsLabel'];
      final bool required = item['required'];
      if ((showsLabel) && (label != null)) {
        result.add(SkyveFormItem(SkyveLabel(
          label,
          required: required,
        )));
      }
      result.add(SkyveFormItem(_one(
        model: item['widget'],
        formLabel: (showsLabel ? label : null),
        required: required,
      )));
    }
    return result;
  }

  static List<SkyveTab> _tabs(List<dynamic> tabs) {
    List<SkyveTab> result = [];
    for (Map<String, dynamic> tab in tabs) {
      result.add(SkyveTab(
          title: tab['title'],
          icon: tab['icon'],
          children: _many(tab['contained'])));
    }
    return result;
  }

  static Validator _createValidators(
      Map<String, dynamic> model, String? formLabel, bool? required) {
    List<Validator> validators = List.empty(growable: true);

    if (required == true) {
      validators.add(RequiredValidator(formLabel));
    }

    if (validators.isEmpty) {
      // No (client-side) validation
      return NoOpValidator();
    } else if (validators.length == 1) {
      // Just the one validator, so we'll return than
      return validators.first;
    } else {
      // Jam all the validators together
      return DelegatingValidator(validators);
    }
  }
}
