import 'package:flutter/material.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:go_router/go_router.dart';
import 'package:skyve_flutter/util/skyve_mixins.dart';

import '../models/payload.dart';
import '../util/skyve_rest_client.dart';
import '../util/skyve_flutter_form.dart';

typedef BeanSupplier = Payload Function();

class SkyveButton extends StatelessWidget with Sizable {
  static const List<String> _poppingActions = ['OK', 'Remove'];

  final String actionType;
  final String actionName;
  final String label;
  final bool clientValidation;
  final bool inActionPanel;

  SkyveButton(
      {Key? key,
      int? pixelWidth,
      int? pixelHeight,
      int? minPixelHeight,
      int? maxPixelHeight,
      required this.actionType,
      required this.actionName,
      required this.label,
      this.clientValidation = false,
      this.inActionPanel = false})
      : super(key: key) {
    this.pixelWidth = pixelWidth;
    this.pixelHeight = pixelHeight;
    this.minPixelHeight = minPixelHeight;
    this.maxPixelHeight = maxPixelHeight;
  }

  @override
  Widget build(BuildContext context) {
    Widget result;
    IconData icon;
    switch (actionType) {
      case 'OK':
        icon = FontAwesomeIcons.check;
        break;
      case 'Save':
        icon = FontAwesomeIcons.floppyDisk;
        break;
      case 'Delete':
        icon = FontAwesomeIcons.trash;
        break;
      case 'Add':
        icon = FontAwesomeIcons.plus;
        break;
      case 'ZoomOut':
        icon = FontAwesomeIcons.reply;
        break;
      case 'Cancel':
        icon = FontAwesomeIcons.chevronLeft;
        break;
      case 'Remove':
        icon = FontAwesomeIcons.minus;
        break;
      case 'Edit':
        icon = FontAwesomeIcons.pencil;
        break;
      default:
        icon = FontAwesomeIcons.gear;
    }

    if (actionType == 'Cancel') {
      result = ElevatedButton.icon(
        icon: FaIcon(icon, size: 16.0),
        label: Text(label),
        onPressed: () => _pop(context),
      );
    } else {
      result = ElevatedButton.icon(
        icon: FaIcon(icon, size: 16.0),
        label: Text(label),
        onPressed: () => _onPressed(context),
      );
    }
    if ((pixelWidth != null) || (pixelHeight != null)) {
      result = SizedBox(
        width: pixelWidth?.toDouble(),
        height: pixelHeight?.toDouble(),
        child: result,
      );
    }
    return result;
  }

  void _pop(BuildContext context) {
    final GoRouter router = GoRouter.of(context);
    if (router.canPop()) {
      router.pop();
    } else {
      debugPrint("WARN: Couldn't pop...");
    }
  }

  void _onPressed(BuildContext context) async {
    // Validate the form, saving an continuing with the
    // action if there were no errors
    var form = Form.of(context);

    if (clientValidation) {
      if (!form.validate()) {
        debugPrint('Validation failure, aborting button action');
        return;
      }
    }

    debugPrint('Saving form');
    form.save();
    SkyveFlutterFormState formState = SkyveFlutterForm.of(context);
    debugPrint('Action $actionType pressed for $formState');

    Payload requestPayload = formState.asPayload();

    // Call rest client to peform the update
    Payload responsePayload =
        await SkyveRestClient().addOrUpdate(actionName, requestPayload);

    // If server result was successful, and this is an
    // action we should pop from we might pop here
    bool shouldPop =
        responsePayload.successful && _poppingActions.contains(actionName);
    if (shouldPop) {
      if (context.mounted) {
        _pop(context);
      } else {
        debugPrint('context was no longer mounted when we wanted to pop...');
        // raise error?
      }
    } else {
      formState.applyPayload(responsePayload);
    }
  }
}
