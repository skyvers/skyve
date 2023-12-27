import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

import '../models/payload.dart';
import '../util/skyve_rest_client.dart';
import '../util/skyve_flutter_form.dart';

typedef BeanSupplier = Payload Function();

class SkyveButton extends StatelessWidget {
  static const List<String> _poppingActions = ['OK', 'Remove'];

  final String actionType;
  final String actionName;
  final String label;
  final bool clientValidation;
  final bool inActionPanel;

  const SkyveButton(
      {Key? key,
      required this.actionType,
      required this.actionName,
      required this.label,
      this.clientValidation = false,
      this.inActionPanel = false})
      : super(key: key);

  @override
  Widget build(BuildContext context) {
    if (actionType == 'Cancel') {
      return ElevatedButton(onPressed: () => _pop(context), child: Text(label));
    } else {
      return ElevatedButton(
          onPressed: () => _onPressed(context), child: Text(label));
    }
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
