package org.skyve.impl.metadata.view;

import java.util.ArrayList;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.ActionUnavailableException;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.event.Addable;
import org.skyve.impl.metadata.view.event.Changeable;
import org.skyve.impl.metadata.view.event.Editable;
import org.skyve.impl.metadata.view.event.EventSource;
import org.skyve.impl.metadata.view.event.Focusable;
import org.skyve.impl.metadata.view.event.Removable;
import org.skyve.impl.metadata.view.event.Selectable;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.reference.ActionReference;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.tabular.AbstractDataWidget;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;
import org.skyve.metadata.view.View;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Refuses an action requested of a view when the view, as rendered for a bean, offers the user no
 * reachable trigger for it.
 *
 * <p>A trigger is reachable when, with the view's conditions evaluated against the bean, it is one of:
 * a toolbar action that is in the action panel, visible and enabled;
 * a {@code <button>} whose container is visible and enabled and whose action is visible and enabled;
 * a {@code <link>} with an action reference, outside any data grid or repeater, that is visible in a visible
 * container and whose action is visible, whether or not the action is enabled, as the SmartClient renderer shows
 * such a link from the link's own condition alone; or
 * a {@code <server>} event action in a handler of a visible widget, whatever the conditions of its action
 * (the client can enable the widget, and the event fires regardless of the action); the input widget of a data grid
 * or repeater column counts as visible when its data widget is, as the SmartClient renderer ignores the column
 * widget's own invisible condition.
 * {@code <component>} fragments are walked in place, with their conditions translated to the host view's and the
 * action names of their buttons and {@code <server>} event actions mapped to the host view's; a link keeps its own
 * action name. Their triggers count as the host view's own.
 *
 * <p>A custom action, or a BizExport, BizImport, Download or Upload action, is requested by its resource (class)
 * name, as SmartClient's toolbar, buttons, events and links send it; the PrimeFaces upload sends the action's name,
 * so it reaches the action only when its name is its class. The server then looks the class up by the requested name,
 * so a request by an action's name is refused when the name differs from the class: it cannot pass for one action
 * and run another class of that name. Two actions sharing a class make that class reachable when either is.
 * Built-in actions are requested by their implicit name (see
 * {@link #check(User, Document, View, String, Bean, ImplicitActionName)}).
 * A view with no actions offers none of them.
 *
 * <p>This class only interprets view metadata; permission checks such as
 * {@link User#canExecuteAction(Document, String)} stay with the callers.
 *
 * <p>Side effects: the bean is only used through {@link Bean#evaluateCondition(String)}, which can run application
 * code (condition methods and expressions) that may be costly or read other state. As in every {@link ViewVisitor}
 * walk, shared default input widget metadata has its binding set and then restored while it is visited.
 *
 * <p>Threading: thread-confined. Each entry point creates its own instance for one walk and discards it, so the
 * entry points share no state of this class; the default input widget rebinding above is shared with every other
 * view walk, such as rendering.
 *
 * @since 10.0
 * @see ActionUnavailableException
 */
public final class ActionAvailability extends NoOpViewVisitor {
	/** The bean the view's conditions are evaluated against. */
	private final @Nonnull Bean bean;
	
	/** The requested action's resource (class) name as the request gave it, or the name of the requested implicit action. */
	private final @Nonnull String requestedName;
	
	/** The requested built-in action, or {@code null} when an action is requested by class. */
	private @Nullable ImplicitActionName requestedImplicitName;
	
	/** The binding of the data grid or repeater a row link is requested from, or {@code null} for a form-level request. */
	private @Nullable String gridBinding;
	
	/** The action links found in the visible data widgets bound to {@link #gridBinding}, in visit order. */
	private final @Nonnull List<Link> rowLinks = new ArrayList<>();
	
	/** Whether a reachable trigger outside data widgets has been found for the requested action; {@link #checkRow} ignores it. */
	private boolean reachable;
	
	/** Whether the walk is inside a data grid or repeater; they do not nest. */
	private boolean inDataWidget;
	
	/** Whether the walk is inside a visible data widget bound to {@link #gridBinding}. */
	private boolean collectingRowLinks;
	
	/** Whether the walk is inside a bound column of a data grid or repeater, visiting its input widget. */
	private boolean inBoundColumn;
	
	/**
	 * The widget whose event handler was entered last, or {@code null} before the first.
	 * The walk enters a handler before visiting its event actions, and handlers do not nest, so it is the widget
	 * of any event action being visited.
	 */
	private @Nullable EventSource eventSource;

	/**
	 * Creates a visitor for one walk of a view, for one requested action.
	 *
	 * @param customer the requesting user's customer, which resolves the view; must not be {@code null}
	 * @param document the document owning the view; must not be {@code null}
	 * @param view the view as rendered for the bean; must not be {@code null}
	 * @param uxui the UX/UI that resolves component fragments; must not be {@code null}
	 * @param bean the bean the view's conditions are evaluated against; must not be {@code null}
	 * @param requestedName the requested action's resource (class) name, or the name of the requested implicit action;
	 * 			must not be {@code null}
	 */
	private ActionAvailability(@Nonnull CustomerImpl customer,
								@Nonnull Document document,
								@Nonnull View view,
								@Nonnull String uxui,
								@Nonnull Bean bean,
								@Nonnull String requestedName) {
		super(customer,
				(ModuleImpl) customer.getModule(document.getOwningModuleName()),
				(DocumentImpl) document,
				(ViewImpl) view,
				uxui);
		this.bean = bean;
		this.requestedName = requestedName;
	}

	/**
	 * Refuses a custom or class-based action that the view offers no reachable trigger for.
	 *
	 * <p>Side effects: on refusal, logs a warning naming the user, the document, the action and the bean's bizId.
	 *
	 * <p>Complexity: O(n) time and O(d) stack, where n is the number of widgets and actions in the view including
	 * its expanded component fragments and d is their nesting depth. Each container's conditions are evaluated once,
	 * and at most two more for each trigger of the requested action; conditions are application-defined and may be
	 * costly.
	 *
	 * @param user the user requesting the action; must not be {@code null}
	 * @param document the document owning the view and the action; must not be {@code null}
	 * @param view the view the user was shown, for the request's UX/UI; must not be {@code null}
	 * @param uxui the request's UX/UI, which resolves component fragments; must not be {@code null}
	 * @param bean the bean the view was rendered for; must not be {@code null}
	 * @param actionName the action's resource (class) name, as the request gave it; must not be {@code null}
	 * @throws ActionUnavailableException if the view offers no reachable trigger for the action
	 */
	public static void check(@Nonnull User user,
								@Nonnull Document document,
								@Nonnull View view,
								@Nonnull String uxui,
								@Nonnull Bean bean,
								@Nonnull String actionName) {
		ActionAvailability availability = new ActionAvailability((CustomerImpl) user.getCustomer(), document, view, uxui, bean, actionName);
		availability.visitAndRefuseUnlessReachable(user);
	}

	/**
	 * Refuses a built-in action that the view offers no reachable trigger for.
	 *
	 * <p>A toolbar action is recognised only for OK, Save and Delete (including those expanded from
	 * {@code <defaults/>}) and the class-based BizExport, BizImport, Download and Upload; other built-ins on the
	 * toolbar, such as ZoomOut or Cancel, are never reachable through it. A button, link or event action is
	 * recognised for whatever built-in its action declares.
	 *
	 * <p>Side effects: on refusal, logs a warning naming the user, the document, the action and the bean's bizId.
	 *
	 * <p>Complexity: O(n) time and O(d) stack, where n is the number of widgets and actions in the view including
	 * its expanded component fragments and d is their nesting depth. Each container's conditions are evaluated once,
	 * and at most two more for each trigger of the requested action; conditions are application-defined and may be
	 * costly.
	 *
	 * @param user the user requesting the action; must not be {@code null}
	 * @param document the document owning the view; must not be {@code null}
	 * @param view the view the user was shown, for the request's UX/UI; must not be {@code null}
	 * @param uxui the request's UX/UI, which resolves component fragments; must not be {@code null}
	 * @param bean the bean the view was rendered for; must not be {@code null}
	 * @param implicitName the requested built-in action; must not be {@code null}
	 * @throws ActionUnavailableException if the view offers no reachable trigger for the action
	 */
	public static void check(@Nonnull User user,
								@Nonnull Document document,
								@Nonnull View view,
								@Nonnull String uxui,
								@Nonnull Bean bean,
								@Nonnull ImplicitActionName implicitName) {
		ActionAvailability availability = new ActionAvailability((CustomerImpl) user.getCustomer(), document, view, uxui, bean, implicitName.name());
		availability.requestedImplicitName = implicitName;
		availability.visitAndRefuseUnlessReachable(user);
	}

	/**
	 * Refuses an action requested from a link in a data grid or repeater row unless such a link is reachable.
	 *
	 * <p>The link is found in the form view and its action is resolved in the row document's view. The request
	 * passes when some data grid or repeater in the form view has the binding {@code gridBinding} and is visible
	 * against {@code formBean}, and one of its container-column links is visible against {@code rowBean} and
	 * references, by the link's action name, an action of {@code rowView} whose resource (class) name is
	 * {@code actionName}. As in the SmartClient renderer, which shows such a link from its own condition alone, the
	 * row action's own conditions are not applied. Every data widget with the binding is considered, wherever it is
	 * nested.
	 * A data widget in a {@code <component>} fragment has the component's binding prefixed to its own, as the
	 * renderers send it, so {@code gridBinding} must include that prefix. Triggers outside the data widgets are not
	 * considered, and their conditions are not evaluated.
	 *
	 * <p>Side effects: on refusal, logs a warning naming the user, both documents, the action, the grid binding and
	 * both bizIds.
	 *
	 * <p>Complexity: O(n + k) time, O(d) stack and O(k) extra space, where n is the number of widgets and actions in
	 * the form view including its expanded component fragments, d is their nesting depth and k is the number of action
	 * links in the matching data widgets. Each container's conditions are evaluated once against the form bean, and
	 * a matching data widget's invisible condition a second time; each collected link's invisible condition is
	 * evaluated at most once against the row bean. Conditions are application-defined and may be costly.
	 *
	 * @param user the user requesting the action; must not be {@code null}
	 * @param formDocument the document owning the form view; must not be {@code null}
	 * @param formView the view the user was shown, for the request's UX/UI; must not be {@code null}
	 * @param uxui the request's UX/UI, which resolves component fragments; must not be {@code null}
	 * @param formBean the bean the form view was rendered for; must not be {@code null}
	 * @param gridBinding the binding of the data grid or repeater the row is in; must not be {@code null}
	 * @param rowDocument the document of the row bean; must not be {@code null}
	 * @param rowView the row document's view that declares the row actions; must not be {@code null}
	 * @param rowBean the bean of the row the link was clicked in; must not be {@code null}
	 * @param actionName the row action's resource (class) name, as the request gave it; must not be {@code null}
	 * @throws ActionUnavailableException if no row link for the action is reachable
	 */
	@SuppressWarnings("java:S107") // The row request needs both the form context and the row context.
	public static void checkRow(@Nonnull User user,
									@Nonnull Document formDocument,
									@Nonnull View formView,
									@Nonnull String uxui,
									@Nonnull Bean formBean,
									@Nonnull String gridBinding,
									@Nonnull Document rowDocument,
									@Nonnull View rowView,
									@Nonnull Bean rowBean,
									@Nonnull String actionName) {
		ActionAvailability availability = new ActionAvailability((CustomerImpl) user.getCustomer(), formDocument, formView, uxui, formBean, actionName);
		availability.gridBinding = gridBinding;
		availability.visit();
		if (! availability.isRowLinkReachable(rowView, rowBean)) {
			availability.LOGGER.warn("Refused action {} for user {} on {}.{} with bizId {} in grid {} of {}.{} with bizId {}: the view offers no reachable trigger",
										actionName,
										user.getName(),
										rowDocument.getOwningModuleName(),
										rowDocument.getName(),
										rowBean.getBizId(),
										gridBinding,
										formDocument.getOwningModuleName(),
										formDocument.getName(),
										formBean.getBizId());
			throw new ActionUnavailableException(user, rowDocument.getName(), actionName);
		}
	}

	/**
	 * Walks the view and refuses the requested form-level action unless a reachable trigger was found.
	 *
	 * <p>Side effects: on refusal, logs a warning naming the user, the document, the action and the bean's bizId.
	 *
	 * @param user the user requesting the action; must not be {@code null}
	 * @throws ActionUnavailableException if the view offers no reachable trigger for the action
	 */
	private void visitAndRefuseUnlessReachable(@Nonnull User user) {
		visit();
		if (! reachable) {
			LOGGER.warn("Refused action {} for user {} on {}.{} with bizId {}: the view offers no reachable trigger",
							requestedName,
							user.getName(),
							document.getOwningModuleName(),
							document.getName(),
							bean.getBizId());
			throw new ActionUnavailableException(user, document.getName(), requestedName);
		}
	}

	/**
	 * Decides whether one of the collected row links reaches the requested action in the row document's view.
	 *
	 * <p>Complexity: O(k) time, where k is the number of collected row links.
	 *
	 * @param rowView the row document's view that declares the row actions; must not be {@code null}
	 * @param rowBean the row bean that the link conditions are evaluated against; must not be {@code null}
	 * @return {@code true} if a link visible for the row references a row action with the requested class
	 */
	private boolean isRowLinkReachable(@Nonnull View rowView, @Nonnull Bean rowBean) {
		for (Link link : rowLinks) {
			// Rule: a row link is a trigger when it is visible for the row; its row action's conditions are not applied
			if (evaluateConditionInOppositeSense(rowBean, link.getInvisibleCondition())) {
				Action action = rowView.getAction(((ActionReference) link.getReference()).getActionName());
				if ((action != null) && requestedName.equals(action.getResourceName())) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Decides whether a form-view action is the one requested.
	 *
	 * <p>A row request ({@link #checkRow}) matches none, so the form-level rules evaluate no conditions for it.
	 * A built-in request matches the same implicit action. A request by class matches a custom or class-based action
	 * whose resource (class) name it is, never by the action's own name.
	 *
	 * @param action the action metadata; must not be {@code null}
	 * @param actionImplicitName the implicit action the action stands for here, or {@code null} for a custom action
	 * @return {@code true} if the action is the requested one
	 */
	private boolean isRequested(@Nonnull Action action, @Nullable ImplicitActionName actionImplicitName) {
		if (gridBinding != null) {
			return false;
		}
		if (requestedImplicitName != null) {
			return requestedImplicitName == actionImplicitName;
		}
		return requestedName.equals(action.getResourceName());
	}

	/**
	 * Evaluates a negative condition, such as an invisible or disabled condition, in the positive sense.
	 *
	 * @param conditionBean the bean to evaluate the condition against; must not be {@code null}
	 * @param condition the condition, or {@code null} when none was declared
	 * @return {@code true} if there is no condition or it is false for the bean
	 */
	private static boolean evaluateConditionInOppositeSense(@Nonnull Bean conditionBean, @Nullable String condition) {
		return (condition == null) || (! conditionBean.evaluateCondition(condition));
	}

	/**
	 * Determines whether metadata is visible, by evaluating its invisible condition against the bean.
	 */
	@Override
	protected boolean visible(@Nonnull Invisible invisible) {
		return evaluateConditionInOppositeSense(bean, invisible.getInvisibleCondition());
	}

	/**
	 * Determines whether metadata is enabled, by evaluating its disabled condition against the bean.
	 */
	@Override
	protected boolean enabled(@Nonnull Disableable disableable) {
		return evaluateConditionInOppositeSense(bean, disableable.getDisabledCondition());
	}

	/**
	 * Records a toolbar action as reachable when it is the requested action.
	 *
	 * @param action the action metadata; must not be {@code null}
	 * @param actionImplicitName the implicit action the action stands for here, or {@code null} for a custom action
	 */
	private void visitToolbarAction(@Nonnull ActionImpl action, @Nullable ImplicitActionName actionImplicitName) {
		// Rule: a toolbar action is a trigger when it is in the action panel, visible and enabled
		if (isRequested(action, actionImplicitName) &&
				(! Boolean.FALSE.equals(action.getInActionPanel())) &&
				visible(action) &&
				enabled(action)) {
			reachable = true;
		}
	}

	/**
	 * Records the toolbar custom action as reachable when it is requested, available and in the action panel.
	 */
	@Override
	public void visitCustomAction(@Nonnull ActionImpl action) {
		visitToolbarAction(action, null);
	}

	/**
	 * Records the toolbar OK action, declared or expanded from {@code <defaults/>}, as reachable when it is requested,
	 * available and in the action panel.
	 *
	 * @param action the declaring action metadata, which may be the {@code <defaults/>} action; must not be {@code null}
	 */
	@Override
	public void visitOKAction(@Nonnull ActionImpl action) {
		visitToolbarAction(action, ImplicitActionName.OK);
	}

	/**
	 * Records the toolbar Save action, declared or expanded from {@code <defaults/>}, as reachable when it is requested,
	 * available and in the action panel.
	 *
	 * @param action the declaring action metadata, which may be the {@code <defaults/>} action; must not be {@code null}
	 */
	@Override
	public void visitSaveAction(@Nonnull ActionImpl action) {
		visitToolbarAction(action, ImplicitActionName.Save);
	}

	/**
	 * Records the toolbar Delete action, declared or expanded from {@code <defaults/>}, as reachable when it is requested,
	 * available and in the action panel.
	 *
	 * @param action the declaring action metadata, which may be the {@code <defaults/>} action; must not be {@code null}
	 */
	@Override
	public void visitDeleteAction(@Nonnull ActionImpl action) {
		visitToolbarAction(action, ImplicitActionName.Delete);
	}

	/**
	 * Records the toolbar BizExport action as reachable when it is requested, available and in the action panel.
	 */
	@Override
	public void visitBizExportAction(@Nonnull ActionImpl action) {
		visitToolbarAction(action, ImplicitActionName.BizExport);
	}

	/**
	 * Records the toolbar BizImport action as reachable when it is requested, available and in the action panel.
	 */
	@Override
	public void visitBizImportAction(@Nonnull ActionImpl action) {
		visitToolbarAction(action, ImplicitActionName.BizImport);
	}

	/**
	 * Records the toolbar Download action as reachable when it is requested, available and in the action panel.
	 */
	@Override
	public void visitDownloadAction(@Nonnull ActionImpl action) {
		visitToolbarAction(action, ImplicitActionName.Download);
	}

	/**
	 * Records the toolbar Upload action as reachable when it is requested, available and in the action panel.
	 */
	@Override
	public void visitUploadAction(@Nonnull ActionImpl action) {
		visitToolbarAction(action, ImplicitActionName.Upload);
	}

	/**
	 * Records the button's action as reachable when it is requested and the button can be clicked.
	 */
	@Override
	public void visitButton(@Nonnull Button button, boolean parentVisible, boolean parentEnabled) {
		Action action = view.getAction(button.getActionName());
		// Rule: a button is a trigger when its container is visible and enabled and its action is visible and enabled
		if (isRequested(action, action.getImplicitName()) &&
				parentVisible &&
				parentEnabled &&
				visible(action) &&
				enabled(action)) {
			reachable = true;
		}
	}

	/**
	 * Records the action of a link outside data widgets as reachable when it is requested and the link is shown,
	 * whether or not the action is enabled, or collects a link in a data widget for {@link #checkRow}.
	 *
	 * @param link the link metadata; must not be {@code null}
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled; ignored
	 */
	@Override
	public void visitLink(@Nonnull Link link, boolean parentVisible, boolean parentEnabled) {
		if (link.getReference() instanceof ActionReference reference) {
			if (inDataWidget) {
				if (collectingRowLinks) {
					rowLinks.add(link);
				}
			}
			else {
				Action action = view.getAction(reference.getActionName());
				// Rule: a link is a trigger when it is visible in a visible container and its action is visible, enabled or not
				if ((action != null) &&
						isRequested(action, action.getImplicitName()) &&
						parentVisible &&
						visible(link) &&
						visible(action)) {
					reachable = true;
				}
			}
		}
	}

	/**
	 * Enters a data grid, collecting its row links when it is the visible grid of a {@link #checkRow} request.
	 */
	@Override
	public void visitDataGrid(@Nonnull DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		visitDataWidget(grid, parentVisible);
	}

	/**
	 * Leaves a data grid, ending row-link collection.
	 */
	@Override
	public void visitedDataGrid(@Nonnull DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		visitedDataWidget();
	}

	/**
	 * Enters a data repeater, collecting its row links when it is the visible repeater of a {@link #checkRow} request.
	 */
	@Override
	public void visitDataRepeater(@Nonnull DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		visitDataWidget(repeater, parentVisible);
	}

	/**
	 * Leaves a data repeater, ending row-link collection.
	 */
	@Override
	public void visitedDataRepeater(@Nonnull DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		visitedDataWidget();
	}

	/**
	 * Enters a bound column, so its input widget's event actions take the data widget's visibility rather than the
	 * widget's own.
	 */
	@Override
	public void visitDataGridBoundColumn(@Nonnull DataGridBoundColumn column, boolean parentVisible, boolean parentEnabled) {
		inBoundColumn = true;
	}

	/**
	 * Leaves a bound column, so later event actions again need their own widget to be visible.
	 */
	@Override
	public void visitedDataGridBoundColumn(@Nonnull DataGridBoundColumn column, boolean parentVisible, boolean parentEnabled) {
		inBoundColumn = false;
	}

	/**
	 * Enters a data widget, whose container-column links act on its rows rather than the form.
	 *
	 * @param widget the data grid or repeater metadata; must not be {@code null}
	 * @param parentVisible whether all ancestor metadata is visible
	 */
	private void visitDataWidget(@Nonnull AbstractDataWidget widget, boolean parentVisible) {
		inDataWidget = true;
		// Rule: row links count only in a visible data widget with the requested grid binding
		collectingRowLinks = (gridBinding != null) &&
								gridBinding.equals(widget.getBinding()) &&
								parentVisible &&
								visible(widget);
	}

	/**
	 * Leaves a data widget.
	 */
	private void visitedDataWidget() {
		inDataWidget = false;
		collectingRowLinks = false;
	}

	/**
	 * Records the event action's action as reachable when it is requested and the handler's widget is visible.
	 *
	 * @param server the server-side event action metadata; must not be {@code null}
	 * @param parentVisible whether the containers of the handler's widget are visible, which for a column's input
	 * 			widget is whether its data widget is visible
	 * @param parentEnabled whether the containers of the handler's widget are enabled; ignored
	 */
	@Override
	public void visitServerSideActionEventAction(@Nonnull ServerSideActionEventAction server, boolean parentVisible, boolean parentEnabled) {
		Action action = view.getAction(server.getActionName());
		// Rule: an event action is a trigger when its widget (in a grid column: the grid) is visible, whatever its action
		if (isRequested(action, action.getImplicitName()) &&
				parentVisible &&
				(inBoundColumn || (! (eventSource instanceof Invisible invisible)) || visible(invisible))) {
			reachable = true;
		}
	}

	/**
	 * Records the widget as the source of the event actions that follow.
	 */
	@Override
	public void visitOnChangedEventHandler(@Nonnull Changeable changeable, boolean parentVisible, boolean parentEnabled) {
		eventSource = changeable;
	}

	/**
	 * Records the widget as the source of the event actions that follow.
	 */
	@Override
	public void visitOnFocusEventHandler(@Nonnull Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		eventSource = blurable;
	}

	/**
	 * Records the widget as the source of the event actions that follow.
	 */
	@Override
	public void visitOnBlurEventHandler(@Nonnull Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		eventSource = blurable;
	}

	/**
	 * Records the widget as the source of the event actions that follow.
	 */
	@Override
	public void visitOnAddedEventHandler(@Nonnull Addable addable, boolean parentVisible, boolean parentEnabled) {
		eventSource = addable;
	}

	/**
	 * Records the widget as the source of the event actions that follow.
	 */
	@Override
	public void visitOnEditedEventHandler(@Nonnull Editable editable, boolean parentVisible, boolean parentEnabled) {
		eventSource = editable;
	}

	/**
	 * Records the widget as the source of the event actions that follow.
	 */
	@Override
	public void visitOnRemovedEventHandler(@Nonnull Removable removable, boolean parentVisible, boolean parentEnabled) {
		eventSource = removable;
	}

	/**
	 * Records the widget as the source of the event actions that follow.
	 */
	@Override
	public void visitOnSelectedEventHandler(@Nonnull Selectable selectable, boolean parentVisible, boolean parentEnabled) {
		eventSource = selectable;
	}

	/**
	 * Records the widget as the source of the event actions that follow.
	 */
	@Override
	public void visitOnPickedEventHandler(@Nonnull LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		eventSource = lookup;
	}

	/**
	 * Records the widget as the source of the event actions that follow.
	 */
	@Override
	public void visitOnClearedEventHandler(@Nonnull LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		eventSource = lookup;
	}
}
