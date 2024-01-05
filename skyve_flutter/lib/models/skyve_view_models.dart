import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/skyve_enums.dart';
import 'package:skyve_flutter/util/skyve_interfaces.dart';
import 'package:skyve_flutter/widgets/skyve_chart.dart';
import 'package:skyve_flutter/widgets/skyve_checkbox.dart';
import 'package:skyve_flutter/widgets/skyve_colourpicker.dart';
import 'package:skyve_flutter/widgets/skyve_combo.dart';
import 'package:skyve_flutter/widgets/skyve_comparison.dart';
import 'package:skyve_flutter/widgets/skyve_contentlink.dart';
import 'package:skyve_flutter/widgets/skyve_contentsignature.dart';
import 'package:skyve_flutter/widgets/skyve_datagrid.dart';
import 'package:skyve_flutter/widgets/skyve_datarepeater.dart';
import 'package:skyve_flutter/widgets/skyve_dynamicimage.dart';
import 'package:skyve_flutter/widgets/skyve_formcolumn.dart';
import 'package:skyve_flutter/widgets/skyve_geometry.dart';
import 'package:skyve_flutter/widgets/skyve_geometrymap.dart';
import 'package:skyve_flutter/widgets/skyve_html.dart';
import 'package:skyve_flutter/widgets/skyve_label.dart';
import 'package:skyve_flutter/widgets/skyve_link.dart';
import 'package:skyve_flutter/widgets/skyve_listgrid.dart';
import 'package:skyve_flutter/widgets/skyve_listmembership.dart';
import 'package:skyve_flutter/widgets/skyve_listrepeater.dart';
import 'package:skyve_flutter/widgets/skyve_lookupdescription.dart';
import 'package:skyve_flutter/widgets/skyve_map.dart';
import 'package:skyve_flutter/widgets/skyve_radio.dart';
import 'package:skyve_flutter/widgets/skyve_richtext.dart';
import 'package:skyve_flutter/widgets/skyve_slider.dart';
import 'package:skyve_flutter/widgets/skyve_spacer.dart';
import 'package:skyve_flutter/widgets/skyve_spinner.dart';
import 'package:skyve_flutter/widgets/skyve_staticimage.dart';
import 'package:skyve_flutter/widgets/skyve_textarea.dart';
import 'package:skyve_flutter/widgets/skyve_treegrid.dart';
import 'package:skyve_flutter/widgets/skyve_zoomin.dart';
import 'package:skyve_flutter/widgets/text_fields/decimal2_field.dart';
import '../util/validators.dart';
import '../widgets/text_fields/int_text_field.dart';
import '../widgets/skyve_tab.dart';
import '../widgets/skyve_tabpane.dart';
import '../widgets/skyve_blurb.dart';
import '../widgets/skyve_button.dart';
import '../widgets/skyve_contentimage.dart';
import '../widgets/skyve_form.dart';
import '../widgets/skyve_formitem.dart';
import '../widgets/skyve_formrow.dart';
import '../widgets/skyve_hbox.dart';
import '../widgets/skyve_textfield.dart';
import '../widgets/skyve_vbox.dart';

class SkyveViewModel implements SkyveAbstractEditView {
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

  static SkyveButton _createButton(Map<String, dynamic> buttonOrAction) {
    final String actionType =
        buttonOrAction['actionType'] ?? buttonOrAction['type'];
    final String actionName = buttonOrAction['actionName'];
    final String label = buttonOrAction['label'];
    final bool clientValidation = buttonOrAction['clientValidation'] ?? false;

    return SkyveButton(
      actionType: actionType,
      actionName: actionName,
      label: label,
      clientValidation: clientValidation,
      pixelWidth: buttonOrAction['pixelWidth'],
      pixelHeight: buttonOrAction['pixelHeight'],
      minPixelHeight: buttonOrAction['minPixelHeight'],
      maxPixelHeight: buttonOrAction['maxPixelHeight'],
    );
  }

  @override
  List<Widget> contained(BuildContext context) {
    return _many(jsonMetaData['contained']);
  }

  static List<Widget> _many(List<dynamic> contained) {
    List<Widget> result = List.empty(growable: true);
    for (dynamic element in contained) {
      Widget? widget = _one(model: element);
      if (widget != null) {
        result.add(widget);
      }
    }
    return result;
  }

  static Widget? _one(
      {required Map<String, dynamic> model,
      String? formLabel,
      bool required = false}) {
    String nullSafeFormLabel = formLabel ?? '';
    final String type = model['type'];
    switch (type) {
      case 'blurb': // form or container parent
        return SkyveBlurb(
            label: model['markup'],
            pixelWidth: model['pixelWidth'],
            pixelHeight: model['pixelHeight']);
      case 'button': // form or container parent
        return _createButton(model);
      case 'chart': // container parent
        return SkyveChart(
            pixelWidth: model['pixelWidth'],
            percentageWidth: model['percentageWidth'],
            responsiveWidth: model['responsiveWidth'],
            sm: model['sm'],
            md: model['md'],
            lg: model['lg'],
            xl: model['xl'],
            minPixelWidth: model['minPixelWidth'],
            maxPixelWidth: model['maxPixelWidth'],
            pixelHeight: model['pixelHeight'],
            percentageHeight: model['percentageHeight'],
            minPixelHeight: model['minPixelHeight'],
            maxPixelHeight: model['maxPixelHeight']);
      case 'checkBox': // form parent
        return SkyveCheckBox(
            label: nullSafeFormLabel,
            tristate: model['triState'] ?? false,
            pixelWidth: model['pixelWidth'],
            pixelHeight: model['pixelHeight']);
      case 'checkMembership': // container parent
        return null;
      case 'colour': // form parent
        return SkyveColourPicker(
            label: nullSafeFormLabel, pixelWidth: model['pixelWidth']);
      case 'combo': // form parent
        return SkyveCombo(
            label: nullSafeFormLabel, pixelWidth: model['pixelWidth']);
      case 'comparison': // container parent
        return SkyveComparison(
            pixelWidth: model['pixelWidth'],
            percentageWidth: model['percentageWidth'],
            responsiveWidth: model['responsiveWidth'],
            sm: model['sm'],
            md: model['md'],
            lg: model['lg'],
            xl: model['xl'],
            minPixelWidth: model['minPixelWidth'],
            maxPixelWidth: model['maxPixelWidth'],
            pixelHeight: model['pixelHeight'],
            percentageHeight: model['percentageHeight'],
            minPixelHeight: model['minPixelHeight'],
            maxPixelHeight: model['maxPixelHeight']);
      case 'contentImage': // form parent
        return SkyveContentImage(
          propertyKey: model['binding'],
          label: nullSafeFormLabel,
          pixelWidth: model['pixelWidth'],
          percentageWidth: model['percentageWidth'],
          responsiveWidth: model['responsiveWidth'],
          sm: model['sm'],
          md: model['md'],
          lg: model['lg'],
          xl: model['xl'],
          minPixelWidth: model['minPixelWidth'],
          maxPixelWidth: model['maxPixelWidth'],
          pixelHeight: model['pixelHeight'],
          percentageHeight: model['percentageHeight'],
          minPixelHeight: model['minPixelHeight'],
          maxPixelHeight: model['maxPixelHeight'],
        );
      case 'contentLink': // form parent
        return SkyveContentLink(
            label: nullSafeFormLabel, pixelWidth: model['pixelWidth']);
      case 'contentSignature': // form parent
        return SkyveContentSignature(
            label: nullSafeFormLabel,
            pixelWidth: model['pixelWidth'],
            pixelHeight: model['pixelHeight']);
      case 'dataGrid': // container parent
        return SkyveDataGrid(
            title: model['title'],
            pixelWidth: model['pixelWidth'],
            percentageWidth: model['percentageWidth'],
            responsiveWidth: model['responsiveWidth'],
            sm: model['sm'],
            md: model['md'],
            lg: model['lg'],
            xl: model['xl'],
            minPixelWidth: model['minPixelWidth'],
            maxPixelWidth: model['maxPixelWidth'],
            pixelHeight: model['pixelHeight'],
            percentageHeight: model['percentageHeight'],
            minPixelHeight: model['minPixelHeight'],
            maxPixelHeight: model['maxPixelHeight']);
      case 'dataRepeater': // container parent
        return SkyveDataRepeater(
            pixelWidth: model['pixelWidth'],
            percentageWidth: model['percentageWidth'],
            responsiveWidth: model['responsiveWidth'],
            sm: model['sm'],
            md: model['md'],
            lg: model['lg'],
            xl: model['xl'],
            minPixelWidth: model['minPixelWidth'],
            maxPixelWidth: model['maxPixelWidth'],
            pixelHeight: model['pixelHeight'],
            percentageHeight: model['percentageHeight'],
            minPixelHeight: model['minPixelHeight'],
            maxPixelHeight: model['maxPixelHeight']);
      case 'dialogButton': // form or container parent
        return null;
      case 'dyanmicImage': // container parent
        return SkyveDynamicImage(
            pixelWidth: model['pixelWidth'],
            percentageWidth: model['percentageWidth'],
            responsiveWidth: model['responsiveWidth'],
            sm: model['sm'],
            md: model['md'],
            lg: model['lg'],
            xl: model['xl'],
            minPixelWidth: model['minPixelWidth'],
            maxPixelWidth: model['maxPixelWidth'],
            pixelHeight: model['pixelHeight'],
            percentageHeight: model['percentageHeight'],
            minPixelHeight: model['minPixelHeight'],
            maxPixelHeight: model['maxPixelHeight']);
      case 'form': // container parent
        {
          List<SkyveFormRow> formRows = _formRows(model['rows']);
          List<SkyveFormColumn> formCols = _formCols(model['columns']);
          return SkyveForm(
              pixelWidth: model['pixelWidth'],
              percentageWidth: model['percentageWidth'],
              responsiveWidth: model['responsiveWidth'],
              sm: model['sm'],
              md: model['md'],
              lg: model['lg'],
              xl: model['xl'],
              minPixelWidth: model['minPixelWidth'],
              maxPixelWidth: model['maxPixelWidth'],
              pixelHeight: model['pixelHeight'],
              percentageHeight: model['percentageHeight'],
              minPixelHeight: model['minPixelHeight'],
              maxPixelHeight: model['maxPixelHeight'],
              border: model['border'],
              borderTitle: model['borderTitle'],
              formCols: formCols,
              formRows: formRows);
        }
      case 'geometry': // form parent
        return SkyveGeometry(
            label: nullSafeFormLabel, pixelWidth: model['pixelWidth']);
      case 'geometryMap': // form parent
        return SkyveGeometryMap(
            label: nullSafeFormLabel,
            pixelWidth: model['pixelWidth'],
            percentageWidth: model['percentageWidth'],
            responsiveWidth: model['responsiveWidth'],
            sm: model['sm'],
            md: model['md'],
            lg: model['lg'],
            xl: model['xl'],
            minPixelWidth: model['minPixelWidth'],
            maxPixelWidth: model['maxPixelWidth'],
            pixelHeight: model['pixelHeight'],
            percentageHeight: model['percentageHeight'],
            minPixelHeight: model['minPixelHeight'],
            maxPixelHeight: model['maxPixelHeight']);
      case 'hbox': // container parent
        return SkyveHBox(
            pixelWidth: model['pixelWidth'],
            percentageWidth: model['percentageWidth'],
            responsiveWidth: model['responsiveWidth'],
            sm: model['sm'],
            md: model['md'],
            lg: model['lg'],
            xl: model['xl'],
            minPixelWidth: model['minPixelWidth'],
            maxPixelWidth: model['maxPixelWidth'],
            pixelHeight: model['pixelHeight'],
            percentageHeight: model['percentageHeight'],
            minPixelHeight: model['minPixelHeight'],
            maxPixelHeight: model['maxPixelHeight'],
            border: model['border'],
            borderTitle: model['borderTitle'],
            children: _many(model['contained']));
      case 'html': // form parent
        return SkyveHTML(
            label: nullSafeFormLabel,
            pixelWidth: model['pixelWidth'],
            pixelHeight: model['pixelHeight']);
      case 'inject': // form or container parent
        return null;
      case 'label': // form or container parent
        return SkyveLabel(
            'Label: v=${model['value']} f=${model['for']} b=${model['binding']}',
            pixelWidth: model['pixelWidth'],
            pixelHeight: model['pixelHeight']);
      case 'link': // form or container parent
        return SkyveLink(
            label: nullSafeFormLabel, pixelWidth: model['pixelWidth']);
      case 'listGrid': // container parent
        return SkyveListGrid(
            title: model['title'],
            pixelWidth: model['pixelWidth'],
            percentageWidth: model['percentageWidth'],
            responsiveWidth: model['responsiveWidth'],
            sm: model['sm'],
            md: model['md'],
            lg: model['lg'],
            xl: model['xl'],
            minPixelWidth: model['minPixelWidth'],
            maxPixelWidth: model['maxPixelWidth'],
            pixelHeight: model['pixelHeight'],
            percentageHeight: model['percentageHeight'],
            minPixelHeight: model['minPixelHeight'],
            maxPixelHeight: model['maxPixelHeight']);
      case 'listRepeater': // container parent
        return SkyveListRepeater(
            pixelWidth: model['pixelWidth'],
            percentageWidth: model['percentageWidth'],
            responsiveWidth: model['responsiveWidth'],
            sm: model['sm'],
            md: model['md'],
            lg: model['lg'],
            xl: model['xl'],
            minPixelWidth: model['minPixelWidth'],
            maxPixelWidth: model['maxPixelWidth'],
            pixelHeight: model['pixelHeight'],
            percentageHeight: model['percentageHeight'],
            minPixelHeight: model['minPixelHeight'],
            maxPixelHeight: model['maxPixelHeight']);
      case 'listMembership': // container parent
        return SkyveListMembership(
            candidatesHeading: model['candidatesHeading'],
            membersHeading: model['membersHeading'],
            pixelWidth: model['pixelWidth'],
            minPixelHeight: model['minPixelHeight']);
      case 'lookupDescription': // form parent
        return SkyveLookupDescription(
            label: nullSafeFormLabel, pixelWidth: model['pixelWidth']);
      case 'map': // container parent
        return SkyveMap(
            pixelWidth: model['pixelWidth'],
            percentageWidth: model['percentageWidth'],
            responsiveWidth: model['responsiveWidth'],
            sm: model['sm'],
            md: model['md'],
            lg: model['lg'],
            xl: model['xl'],
            minPixelWidth: model['minPixelWidth'],
            maxPixelWidth: model['maxPixelWidth'],
            pixelHeight: model['pixelHeight'],
            percentageHeight: model['percentageHeight'],
            minPixelHeight: model['minPixelHeight'],
            maxPixelHeight: model['maxPixelHeight']);
      case 'password': // form parent
        {
          List<Validator> validators =
              _createValidators(model, formLabel, required);
          return SkyveTextField(
            propertyKey: model['binding'],
            label: formLabel,
            validators: validators,
            obscureText: true,
          );
        }
      case 'progressBar': // form parent
        return null;
      case 'radio': // form parent
        return SkyveRadio(
            label: nullSafeFormLabel, pixelWidth: model['pixelWidth']);
      case 'richText': // form parent
        return SkyveRichText(
            label: nullSafeFormLabel,
            pixelWidth: model['pixelWidth'],
            pixelHeight: model['pixelHeight'],
            minPixelHeight: model['minPixelHeight'],
            maxPixelHeight: model['maxPixelHeight']);
      case 'slider': // form parent
        return SkyveSlider(
            label: nullSafeFormLabel,
            pixelWidth: model['pixelWidth'],
            pixelHeight: model['pixelHeight'],
            minPixelHeight: model['minPixelHeight'],
            maxPixelHeight: model['maxPixelHeight']);
      case 'spacer': // form or container parent
        return SkyveSpacer(
            pixelWidth: model['pixelWidth'], pixelHeight: model['pixelHeight']);
      case 'spinner': // form parent
        return SkyveSpinner(
            label: nullSafeFormLabel, pixelWidth: model['pixelWidth']);
      case 'staticImage': // form or container parent
        return SkyveStaticImage(
            pixelWidth: model['pixelWidth'],
            percentageWidth: model['percentageWidth'],
            responsiveWidth: model['responsiveWidth'],
            sm: model['sm'],
            md: model['md'],
            lg: model['lg'],
            xl: model['xl'],
            minPixelWidth: model['minPixelWidth'],
            maxPixelWidth: model['maxPixelWidth'],
            pixelHeight: model['pixelHeight'],
            percentageHeight: model['percentageHeight'],
            minPixelHeight: model['minPixelHeight'],
            maxPixelHeight: model['maxPixelHeight']);
      case 'tabPane': // container parent
        return SkyveTabPane(tabs: _tabs(model['tabs']));
      case 'textArea': // form parent
        return SkyveTextArea(
          label: nullSafeFormLabel,
          pixelWidth: model['pixelWidth'],
          pixelHeight: model['pixelHeight'],
          minPixelHeight: model['minPixelHeight'],
        );
      case 'textField': // form parent
        {
          List<Validator> validators =
              _createValidators(model, formLabel, required);
          return _createTextField(model, formLabel, validators);
        }
      case 'treeGrid': // container parent
        return SkyveTreeGrid(
            title: model['title'],
            pixelWidth: model['pixelWidth'],
            percentageWidth: model['percentageWidth'],
            responsiveWidth: model['responsiveWidth'],
            sm: model['sm'],
            md: model['md'],
            lg: model['lg'],
            xl: model['xl'],
            minPixelWidth: model['minPixelWidth'],
            maxPixelWidth: model['maxPixelWidth'],
            pixelHeight: model['pixelHeight'],
            percentageHeight: model['percentageHeight'],
            minPixelHeight: model['minPixelHeight'],
            maxPixelHeight: model['maxPixelHeight']);
      case 'vbox': // container parent
        return SkyveVBox(
            pixelWidth: model['pixelWidth'],
            percentageWidth: model['percentageWidth'],
            responsiveWidth: model['responsiveWidth'],
            sm: model['sm'],
            md: model['md'],
            lg: model['lg'],
            xl: model['xl'],
            minPixelWidth: model['minPixelWidth'],
            maxPixelWidth: model['maxPixelWidth'],
            pixelHeight: model['pixelHeight'],
            percentageHeight: model['percentageHeight'],
            minPixelHeight: model['minPixelHeight'],
            maxPixelHeight: model['maxPixelHeight'],
            border: model['border'],
            borderTitle: model['borderTitle'],
            children: _many(model['contained']));
      case 'zoomIn': // form or container parent
        return SkyveZoomIn(
            pixelWidth: model['pixelWidth'],
            pixelHeight: model['pixelHeight'],
            minPixelHeight: model['minPixelHeight'],
            maxPixelHeight: model['maxPixelHeight']);
      default:
        throw ErrorDescription('unsupported $type');
    }
  }

  static Widget _createTextField(Map<String, dynamic> model, String? formLabel,
      List<Validator> validators) {
    // {
    //    "type": "item",
    //    "label": "Decimal 10",
    //    "showsLabel": true,
    //    "required": false,
    //    "widget": {                // <- model should be this element
    //        "type": "textField",
    //        "binding": "decimal10",
    //        "target": {
    //            "attributeType": "decimal10"
    //        }
    //    }
    // }

    Map<String, dynamic> target = model['target'];
    String attributeType = target['attributeType'];

    String propertyKey = model['binding'];
    int? pixelWidth = model['pixelWidth'];

    switch (attributeType) {
      case 'text':
        return SkyveTextField(
          propertyKey: propertyKey,
          label: formLabel,
          validators: validators,
          pixelWidth: pixelWidth,
        );
      case 'integer':
      case 'longInteger':
        return IntTextField(
          propertyKey: propertyKey,
          label: formLabel,
          validators: validators,
          pixelWidth: pixelWidth,
        );
      case 'decimal2':
        return Decimal2Field(
          propertyKey: propertyKey,
          label: formLabel,
          validators: validators,
          pixelWidth: pixelWidth,
        );
      default:
        {
          debugPrint('TODO Unhandled text field attributeType=$attributeType');
          return SkyveTextField(
            propertyKey: propertyKey,
            label: formLabel,
            validators: validators,
            pixelWidth: pixelWidth,
          );
        }
    }
  }

  static List<SkyveFormRow> _formRows(List<dynamic> rows) {
    return rows
        .map((row) => SkyveFormRow(formItems: _formItems(row['items'])))
        .toList();
  }

  static List<SkyveFormColumn> _formCols(List<dynamic> cols) {
    return cols
        .map((col) => SkyveFormColumn(
            pixelWidth: col['pixelWidth'] as int?,
            percentageWidth: col['percentageWidth'] as int?,
            responsiveWidth: col['responsiveWidth'] as int?,
            sm: col['sm'] as int?,
            md: col['md'] as int?,
            lg: col['lg'] as int?,
            xl: col['xl'] as int?))
        .toList();
  }

  static List<SkyveFormItem> _formItems(List<dynamic> items) {
    List<SkyveFormItem> result = [];
    for (Map<String, dynamic> item in items) {
      final String? align = item['align'];
      final String? label = item['label'];
      final bool showLabel = item['showLabel'];
      final String? labelAlign = item['labelAlign'];
      final bool required = item['required'];
      Widget? widget = _one(
        model: item['widget'],
        formLabel: (showLabel ? label : null),
        required: required,
      );
      if (widget != null) {
        result.add(SkyveFormItem(
          widget,
          label: label,
          align:
              (align == null) ? null : HorizontalAlignment.values.byName(align),
          colspan: item['colspan'] ?? 1,
          help: item['help'],
          labelAlign: (labelAlign == null)
              ? null
              : HorizontalAlignment.values.byName(labelAlign),
          required: required,
          rowspan: item['rowspan'] ?? 1,
          showHelp: item['showHelp'] ?? true,
          showLabel: showLabel,
        ));
      }
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

  /// Setup the initial set of validators for a field
  /// This method might not make much sense anymore
  static List<Validator> _createValidators(
      Map<String, dynamic> model, String? formLabel, bool? required) {
    List<Validator> validators = List.empty(growable: true);

    // Is required the only one that works here?
    if (required == true) {
      validators.add(RequiredValidator(formLabel));
    }

    return validators;
  }
}
