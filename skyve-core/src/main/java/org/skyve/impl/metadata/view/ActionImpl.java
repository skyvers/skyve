package org.skyve.impl.metadata.view;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.view.actions.ActionMetaData;
import org.skyve.impl.metadata.repository.view.actions.AddAction;
import org.skyve.impl.metadata.repository.view.actions.BizExportAction;
import org.skyve.impl.metadata.repository.view.actions.BizImportAction;
import org.skyve.impl.metadata.repository.view.actions.CancelAction;
import org.skyve.impl.metadata.repository.view.actions.ClassAction;
import org.skyve.impl.metadata.repository.view.actions.CustomAction;
import org.skyve.impl.metadata.repository.view.actions.DefaultsAction;
import org.skyve.impl.metadata.repository.view.actions.DeleteAction;
import org.skyve.impl.metadata.repository.view.actions.NewAction;
import org.skyve.impl.metadata.repository.view.actions.OKAction;
import org.skyve.impl.metadata.repository.view.actions.ParameterizableAction;
import org.skyve.impl.metadata.repository.view.actions.PositionableAction;
import org.skyve.impl.metadata.repository.view.actions.RemoveAction;
import org.skyve.impl.metadata.repository.view.actions.ReportAction;
import org.skyve.impl.metadata.repository.view.actions.SaveAction;
import org.skyve.impl.metadata.repository.view.actions.UploadAction;
import org.skyve.impl.metadata.repository.view.actions.ZoomOutAction;
import org.skyve.impl.metadata.view.widget.bound.input.ContentCapture;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.report.ReportFormat;

/**
 * Runtime implementation of the {@link Action} metadata interface.
 *
 * <p>Holds the resolved action properties (name, display name, icon, confirmation
 * text, visibility/disability conditions, and parameters) assembled from the
 * JAXB action descriptor during view conversion.
 *
 * <p>Threading: not thread-safe.  Instances are constructed during view bootstrap
 * and are read-only once placed in the repository cache.
 *
 * @see org.skyve.metadata.view.Action
 */
public class ActionImpl implements Action {
	private static final long serialVersionUID = -133387187684800312L;

	private String name;
	private ImplicitActionName implicitName;
	private String resourceName;
	private Boolean clientValidation = Boolean.TRUE;
	private String displayName;
	private String toolTip;
	private String relativeIconFileName;
	private String iconStyleClass;
	private String confirmationText;
	private Boolean inActionPanel = Boolean.TRUE;
	private ActionShow show = ActionShow.both;
	private String disabledConditionName;
	private String invisibleConditionName;
	private List<Parameter> parameters = new ArrayList<>();
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Returns the mutable parameter list supplied to parameterizable actions.
	 *
	 * <p>The returned list is live and never {@code null}.
	 */
	@Override
	public List<Parameter> getParameters() {
		return parameters;
	}

	/**
	 * Resolves the runtime action identifier.
	 *
	 * <p>Resolution order is explicit name, then resource name, then implicit action name.
	 * This guarantees a stable key for action maps after repository conversion.
	 *
	 * @return the effective action name; never {@code null} for a valid action definition
	 */
	@Override
	public String getName() {
		String result = name;
		if (result == null) {
			result = resourceName;
		}
		if (result == null) {
			result = implicitName.name();
		}

		return result;
	}

	/**
	 * Sets the explicit action name used as the runtime identifier.
	 *
	 * @param name the action name; may be {@code null} to fall back to resource or implicit naming
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * Returns the configured implicit action kind.
	 *
	 * @return the implicit action name, or {@code null} for custom actions
	 */
	@Override
	public ImplicitActionName getImplicitName() {
		return implicitName;
	}

	/**
	 * Sets the implicit action kind.
	 *
	 * @param implicitName the implicit action identifier, or {@code null} for custom actions
	 */
	public void setImplicitName(ImplicitActionName implicitName) {
		this.implicitName = implicitName;
	}

	/**
	 * Returns the backing resource name for this action.
	 *
	 * @return the resource name, or {@code null} for implicit actions
	 */
	@Override
	public String getResourceName() {
		return resourceName;
	}

	/**
	 * Sets the backing resource name for this action.
	 *
	 * @param resourceName the resource name used to resolve class/report/server-side action resources
	 */
	public void setResourceName(String resourceName) {
		this.resourceName = resourceName;
	}

	/**
	 * Returns whether client-side validation should execute before action submission.
	 *
	 * @return {@code true} by default, or the configured validation flag
	 */
	@Override
	public Boolean getClientValidation() {
		return clientValidation;
	}

	/**
	 * Sets whether client-side validation should execute before action submission.
	 *
	 * @param clientValidation the client-side validation flag
	 */
	public void setClientValidation(Boolean clientValidation) {
		this.clientValidation = clientValidation;
	}

	/**
	 * Returns the display label shown for this action.
	 *
	 * @return the display name, or {@code null} when default rendering is used
	 */
	@Override
	public String getDisplayName() {
		return displayName;
	}

	/**
	 * Sets the display label shown for this action.
	 *
	 * @param displayName the user-facing action label
	 */
	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	/**
	 * Returns the relative icon file name used for action rendering.
	 *
	 * @return the relative icon file name, or {@code null}
	 */
	@Override
	public String getRelativeIconFileName() {
		return relativeIconFileName;
	}

	/**
	 * Sets the relative icon file name used for action rendering.
	 *
	 * @param relativeIconFileName the icon file path relative to action resource roots
	 */
	public void setRelativeIconFileName(String relativeIconFileName) {
		this.relativeIconFileName = relativeIconFileName;
	}

	/**
	 * Returns the CSS style class used for icon rendering.
	 *
	 * @return the icon style class, or {@code null}
	 */
	@Override
	public String getIconStyleClass() {
		return iconStyleClass;
	}

	/**
	 * Sets the CSS style class used for icon rendering.
	 *
	 * @param iconStyleClass the icon style class name
	 */
	public void setIconStyleClass(String iconStyleClass) {
		this.iconStyleClass = iconStyleClass;
	}

	/**
	 * Returns the confirmation message presented before action execution.
	 *
	 * @return the confirmation text, or {@code null} if no confirmation is required
	 */
	@Override
	public String getConfirmationText() {
		return confirmationText;
	}

	/**
	 * Sets the confirmation message presented before action execution.
	 *
	 * @param confirmationText the confirmation text
	 */
	public void setConfirmationText(String confirmationText) {
		this.confirmationText = confirmationText;
	}

	/**
	 * Returns the tooltip text used for action presentation.
	 *
	 * @return the tooltip text, or {@code null}
	 */
	@Override
	public String getToolTip() {
		return toolTip;
	}

	/**
	 * Sets the tooltip text used for action presentation.
	 *
	 * @param toolTip the tooltip text
	 */
	public void setToolTip(String toolTip) {
		this.toolTip = toolTip;
	}

	/**
	 * Returns whether this action should render in the action panel.
	 *
	 * @return {@code true} by default; {@code false} to render outside the panel where supported
	 */
	@Override
	public Boolean getInActionPanel() {
		return inActionPanel;
	}

	/**
	 * Sets whether this action should render in the action panel.
	 *
	 * @param inActionPanel panel-rendering flag
	 */
	public void setInActionPanel(Boolean inActionPanel) {
		this.inActionPanel = inActionPanel;
	}

	/**
	 * Returns where this action should be shown by default.
	 *
	 * @return the action visibility target
	 */
	@Override
	public ActionShow getShow() {
		return show;
	}

	/**
	 * Sets where this action should be shown by default.
	 *
	 * @param show the action visibility target
	 */
	public void setShow(ActionShow show) {
		this.show = show;
	}

	/**
	 * Resolves the executable server-side action for this metadata action.
	 *
	 * <p>Precondition: this action must reference an explicit resource name.
	 * Implicit actions are not backed by user-defined server-side actions and cause
	 * an {@link IllegalStateException}.
	 *
	 * @param customer the active customer context used for action lookup
	 * @param document the owning document that defines the server-side action
	 * @return the resolved server-side action implementation
	 * @throws IllegalStateException if this action is implicit and has no resource name
	 */
	@Override
	public ServerSideAction<?> getServerSideAction(Customer customer, Document document) {
		if (resourceName == null) {
			throw new IllegalStateException("The ActionConfig " + getName() + " is an implicit action.");
		}

		return document.getServerSideAction(customer, resourceName, true);
	}

	/**
	 * Returns the condition that hides this action when it evaluates to true.
	 *
	 * @return invisible condition name, or {@code null}
	 */
	@Override
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	/**
	 * Sets the condition that hides this action when it evaluates to true.
	 *
	 * @param invisibleConditionName the invisible condition name
	 */
	@Override
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = invisibleConditionName;
	}

	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getVisibleConditionName() {
		return null;
	}

	/**
	 * Stores a visible-condition expression by translating it to the internal invisible-condition form.
	 *
	 * <p>Side effects: updates {@link #invisibleConditionName} with the negated expression.
	 *
	 * @param visibleConditionName the condition that enables visibility semantics
	 */
	@Override
	public void setVisibleConditionName(String visibleConditionName) {
		this.invisibleConditionName = BindUtil.negateCondition(visibleConditionName);
	}

	/**
	 * Returns the condition that disables this action when it evaluates to true.
	 *
	 * @return disabled condition name, or {@code null}
	 */
	@Override
	public String getDisabledConditionName() {
		return disabledConditionName;
	}

	/**
	 * Sets the condition that disables this action when it evaluates to true.
	 *
	 * @param disabledConditionName the disabled condition name
	 */
	@Override
	public void setDisabledConditionName(String disabledConditionName) {
		this.disabledConditionName = disabledConditionName;
	}

	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnabledConditionName() {
		return null;
	}

	/**
	 * Stores an enabled-condition expression by translating it to the internal disabled-condition form.
	 *
	 * <p>Side effects: updates {@link #disabledConditionName} with the negated expression.
	 *
	 * @param enabledConditionName the condition that enables action interactivity
	 */
	@Override
	public void setEnabledConditionName(String enabledConditionName) {
		this.disabledConditionName = BindUtil.negateCondition(enabledConditionName);
	}

	/**
	 * Converts this runtime action representation back to repository action metadata.
	 *
	 * <p>Side effects: creates a new metadata instance, copies common presentation and
	 * condition fields, and applies implicit-action specific translation rules (for example
	 * report parameter extraction and action-panel positioning).
	 *
	 * @return repository action metadata equivalent to this runtime action
	 * @throws IllegalStateException if the implicit action name is unsupported
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	public ActionMetaData toRepositoryAction() {
		ActionMetaData result = null;
		if (implicitName == null) { // custom action
			result = new CustomAction();
			((CustomAction) result).setClientValidation(getClientValidation());
		}
		else {
			switch (implicitName) {
			case Add:
				result = new AddAction();
				break;
			case BizExport:
				result = new BizExportAction();
				break;
			case BizImport:
				result = new BizImportAction();
				break;
			case Cancel:
				result = new CancelAction();
				break;
			case DEFAULTS:
				result = new DefaultsAction();
				break;
			case Delete:
				result = new DeleteAction();
				break;
			case New:
				result = new NewAction();
				break;
			case OK:
				result = new OKAction();
				break;
			case Remove:
				result = new RemoveAction();
				break;
			case Report:
				ReportAction report = new ReportAction();
				String reportName = getResourceName();
				if (reportName == null) {
					reportName = getName();
				}
				report.setReportName(reportName);
				for (Parameter parameter : getParameters()) {
					String parameterName = parameter.getName();
					if (AbstractWebContext.MODULE_NAME.equals(parameterName)) {
						report.setModuleName(parameter.getValue());
					}
					else if (AbstractWebContext.DOCUMENT_NAME.equals(parameterName)) {
						report.setDocumentName(parameter.getValue());
					}
					else if (AbstractWebContext.REPORT_FORMAT.equals(parameterName)) {
						report.setReportFormat(ReportFormat.valueOf(parameter.getValue()));
					}
				}
				result = report;
				break;
			case Save:
				result = new SaveAction();
				break;
			case Upload:
				UploadAction upload = new UploadAction();
				String capture = getProperties().get(UploadAction.CAPTURE_PROPERTY_NAME);
				if (capture != null) {
					upload.setCapture(ContentCapture.valueOf(capture));
				}
				result = upload;
				break;
			case ZoomOut:
				result = new ZoomOutAction();
				break;
			default:
				throw new IllegalStateException(implicitName + " not catered for.");
			}
		}

		result.setConfirmationText(getConfirmationText());
		result.setDisabledConditionName(getDisabledConditionName());
		result.setDisplayName(getDisplayName());
		result.setInvisibleConditionName(getInvisibleConditionName());
		result.setRelativeIconFileName(getRelativeIconFileName());
		result.setIconStyleClass(getIconStyleClass());
		result.setToolTip(getToolTip());

		if (Boolean.FALSE.equals(inActionPanel) && (result instanceof PositionableAction positionableAction)) {
			positionableAction.setInActionPanel(Boolean.FALSE);
		}

		if (result instanceof ClassAction classAction) {
			classAction.setClassName(getResourceName());
		}

		if (result instanceof ParameterizableAction parameterizableAction) {
			parameterizableAction.getParameters().addAll(getParameters());
		}

		return result;
	}

	/**
	 * Replaces the action property map.
	 *
	 * <p>Side effects: subsequent callers of {@link #getProperties()} observe the same
	 * map instance supplied here.
	 */
	public void setProperties(Map<String, String> properties) {
		this.properties = properties;
	}

	/**
	 * Returns the mutable property map for metadata decorators.
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
