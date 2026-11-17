package org.skyve.impl.generate.sail;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.ViewActionPrivileges;
import org.skyve.impl.metadata.view.NoOpViewVisitor;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.interaction.TabSelect;
import org.skyve.metadata.sail.language.step.interaction.TestDataEnter;
import org.skyve.metadata.sail.language.step.interaction.actions.Action;
import org.skyve.metadata.sail.language.step.interaction.actions.ZoomOut;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridNew;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * View visitor that traverses a Skyve view model and emits the steps of a SAIL
 * (Skyve Application Integration Language) test script: populate steps (test data, tab selections and new data grid
 * rows) and action steps (tab selections and actions), which {@link Generator} wraps in navigation, saves and deletes.
 *
 * <p>Only what the user is shown is generated, so that the script never names a widget that its view lacks.
 *
 * <p>Privileges are decided as the renderers decide them ({@link ViewActionPrivileges}): an action the user's privileges do
 * not let the view show is not generated, whether it is a toolbar action or a button's; nor is a new data grid row when
 * the user cannot create the grid's document.
 *
 * <p>Conditions: an action is generated when it is visible and enabled and so are the containers of its button (tab
 * panes, tabs, forms, vboxes, hboxes and the sidebar); a tab selection when the tab and its containers are; a new data
 * grid row when the grid and its containers are and the grid can add; a toolbar action, Save or Delete when it is in the
 * action panel. A condition is evaluated only when a step needs it, and each condition once.
 *
 * <p>{@code true} and {@code false} are constants. {@code persisted}, {@code notPersisted}, {@code changed} and
 * {@code notChanged} change as the script enters data and saves, so their values are not known. Any other condition is
 * evaluated against two instances of the document: the new instance that the script opens
 * ({@link Document#newInstance(User)}) and one holding test data such as {@code <testDataEnter/>} enters (a
 * {@link DataBuilder} with the {@code sail} fixture). When both give the same value, the condition is taken not to
 * depend on what the script enters (it depends on the user, their roles, the configuration or the document's defaults)
 * and its value is used. When they differ, or it cannot be evaluated, its value is not known.
 *
 * <p>A step that a known condition hides or disables is not generated. A step that a condition whose value is not
 * known could hide or disable is not generated either: a comment naming the condition as the view declares it, such as
 * {@code not generated: tab Subscriptions depends on visible="persisted"}, stands in its place, for the author of the
 * script to decide.
 *
 * <p>Conditions are evaluated as the persistence's current user (the Control Panel sets it to the SAIL user while it
 * generates) in the current request, so a condition on the session, such as SAIL recording, reads the generating
 * session. A condition that depends on the saved state or on a random value can still take the same value on both
 * instances by chance; the script's own test data is random too, so such a step can still fail when the script runs.
 *
 * <p>Side effects: the first condition evaluated creates the two instances, which runs the document's Bizlet
 * {@code newInstance} and builds random test data; neither is saved. Conditions can run application code, which can
 * write; the Control Panel rolls back what generation wrote.
 *
 * <p>Threading: not thread-safe; one instance per traversal.
 *
 * <p>Used by {@link Generator} to produce SAIL scripts from existing view metadata.
 */
public class GenerateViewVisitor extends NoOpViewVisitor {
	/** The implicit conditions whose values change as the script enters data and saves. */
	private static final Set<String> SCRIPT_STATE_CONDITIONS = Set.of(Bean.PERSISTED_KEY,
																		Bean.NOT_PERSISTED_KEY,
																		Bean.CHANGED_KEY,
																		Bean.NOT_CHANGED_KEY);

	private final @Nonnull User user;
	private List<Step> populateSteps = new ArrayList<>();
	private List<Step> actionSteps = new ArrayList<>();
	/** The comments that stand in for Delete actions that are not generated, for a generator that would delete. */
	private List<Step> deleteComments = new ArrayList<>();
	private boolean hasSave = false;
	private boolean hasOk = false;
	private boolean hasCancel = false;
	private boolean hasDelete = false;
	// list of module.document.view.binding visit in data grids to stop infinite recursion
	private Set<String> breadcrumbs;
	/** The containers being visited, innermost first; their conditions are evaluated only when a step needs them. */
	private final Deque<Object> containers = new ArrayDeque<>();
	/** What is known of each condition evaluated so far (as a negative condition), by condition. */
	private final Map<String, Availability> conditions = new HashMap<>();
	/** The instances that conditions are evaluated against, or {@code null} before they are first needed. */
	private @Nullable Instances instances;
	/** Why the instances could not be created, or {@code null} unless that failed; it is not tried again. */
	private @Nullable String instancesFailure;

	/**
	 * Creates a visitor for the create view of a document, or its edit view when it has no create view of its own.
	 *
	 * @param user the user the script runs as, whose privileges decide which actions it is shown; must not be
	 * 			{@code null}
	 * @param customer the user's customer; must not be {@code null}
	 * @param module the module whose menu or documents are being generated; must not be {@code null}
	 * @param document the document whose view is visited; must not be {@code null}
	 * @param uxui the UX/UI whose view is visited; must not be {@code null}
	 */
	protected GenerateViewVisitor(@Nonnull User user,
									@Nonnull Customer customer,
									@Nonnull Module module,
									@Nonnull Document document,
									@Nonnull String uxui) {
		this(user, customer, module, document, uxui, new TreeSet<>());
	}
	
	private GenerateViewVisitor(@Nonnull User user,
									@Nonnull Customer customer,
									@Nonnull Module module,
									@Nonnull Document document,
									@Nonnull String uxui,
									@Nonnull Set<String> breadcrumbs) {
		super((CustomerImpl) customer, 
				(ModuleImpl) module,
				(DocumentImpl) document,
				determineView(customer, document, uxui),
				uxui);
		this.user = user;
		this.breadcrumbs = breadcrumbs;
		populateSteps.add(new TestDataEnter());
	}

	private static @Nonnull ViewImpl determineView(@Nonnull Customer c, @Nonnull Document d, @Nonnull String uxui) {
		ViewImpl createView = (ViewImpl) d.getView(uxui, c, ViewType.create.toString());
		ViewImpl editView = (ViewImpl) d.getView(uxui, c, ViewType.edit.toString());
		
		return (createView != editView) ? createView : editView;
	}

	/**
	 * Adds a selection of the tab to the populate and action steps when the tab can be selected, or a comment when
	 * that depends on a condition whose value is not known, and enters the tab for its contents.
	 *
	 * @throws MetaDataException if the tab has no title to select it by
	 */
	@Override
	public void visitTab(@Nonnull Tab tab, boolean parentVisible, boolean parentEnabled) {
		// A tab is selected by its title, which is null when the view metadata gives it a blank title
		String title = tab.getLocalisedTitle();
		if (title == null) {
			throw new MetaDataException("A tab in view " + view.getName() + " of document " + document.getName() + " has no title to select it by");
		}
		containers.push(tab);
		Availability availability = enclosing();
		if (availability.unusable()) {
			return;
		}
		Step step;
		if (availability.condition() == null) {
			TabSelect select = new TabSelect();
			select.setTabPath(title);
			step = select;
		}
		else {
			step = notGenerated("tab " + title, availability);
		}
		populateSteps.add(step);
		actionSteps.add(step);
	}

	/**
	 * Leaves the tab.
	 */
	@Override
	public void visitedTab(@Nonnull Tab tab, boolean parentVisible, boolean parentEnabled) {
		containers.pop();
	}

	/**
	 * Enters the tab pane, whose conditions apply to its tabs.
	 */
	@Override
	public void visitTabPane(@Nonnull TabPane tabPane, boolean parentVisible, boolean parentEnabled) {
		containers.push(tabPane);
	}

	/**
	 * Leaves the tab pane.
	 */
	@Override
	public void visitedTabPane(@Nonnull TabPane tabPane, boolean parentVisible, boolean parentEnabled) {
		containers.pop();
	}

	/**
	 * Enters the vbox, whose condition applies to its contents.
	 */
	@Override
	public void visitVBox(@Nonnull VBox vbox, boolean parentVisible, boolean parentEnabled) {
		containers.push(vbox);
	}

	/**
	 * Leaves the vbox.
	 */
	@Override
	public void visitedVBox(@Nonnull VBox vbox, boolean parentVisible, boolean parentEnabled) {
		containers.pop();
	}

	/**
	 * Enters the hbox, whose condition applies to its contents.
	 */
	@Override
	public void visitHBox(@Nonnull HBox hbox, boolean parentVisible, boolean parentEnabled) {
		containers.push(hbox);
	}

	/**
	 * Leaves the hbox.
	 */
	@Override
	public void visitedHBox(@Nonnull HBox hbox, boolean parentVisible, boolean parentEnabled) {
		containers.pop();
	}

	/**
	 * Enters the form, whose conditions apply to its items.
	 */
	@Override
	public void visitForm(@Nonnull Form form, boolean parentVisible, boolean parentEnabled) {
		containers.push(form);
	}

	/**
	 * Leaves the form.
	 */
	@Override
	public void visitedForm(@Nonnull Form form, boolean parentVisible, boolean parentEnabled) {
		containers.pop();
	}

	/**
	 * Enters the sidebar, whose condition applies to its contents.
	 */
	@Override
	public void visitSidebar(@Nonnull Sidebar sidebar, boolean parentVisible, boolean parentEnabled) {
		containers.push(sidebar);
	}

	/**
	 * Leaves the sidebar.
	 */
	@Override
	public void visitedSidebar(@Nonnull Sidebar sidebar, boolean parentVisible, boolean parentEnabled) {
		containers.pop();
	}
	
	/**
	 * Adds a new row to the populate steps, with the steps that populate it, when the user can create the grid's
	 * document and the grid can add a row; or a comment when that depends on a condition whose value is not known.
	 *
	 * @throws MetaDataException if an addable grid has no binding
	 */
	@Override
	public void visitDataGrid(@Nonnull DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		// Do nothing if we can't add a row
		if (Boolean.FALSE.equals(grid.getEditable()) || Boolean.FALSE.equals(grid.getShowAdd())) {
			return;
		}

		String binding = grid.getBinding();
		if (binding == null) { // validated when the view is loaded
			throw new MetaDataException("A data grid in view " + view.getName() + " of document " + document.getName() + " has no binding");
		}

		TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
		Relation relation = (Relation) target.getAttribute();
		Document gridDocument = (relation == null) ? null : module.getDocument(customer, relation.getDocumentName());
		// The renderers show the grid's New only to a user who can create its document
		if ((gridDocument != null) && (! user.canCreateDocument(gridDocument))) {
			return;
		}

		Availability availability = enclosing().and(availability(grid))
										.and(negative(grid.getDisableAddCondition(),
														authored("enableAdd",
																	grid.getXmlEnableAddCondition(),
																	"disableAdd",
																	grid.getXmlDisableAddCondition())));
		if (availability.unusable()) {
			return;
		}
		if (availability.condition() != null) {
			populateSteps.add(notGenerated("a new row in data grid " + binding, availability));
			return;
		}

		// Do nothing if we've visited this grid on this view before
		String breadcrumb = String.format("%s.%s.%s.%s", module.getName(), document.getName(), view.getName(), binding);
		if (! breadcrumbs.add(breadcrumb)) {
			return;
		}
		
		DataGridNew nu = new DataGridNew();
		nu.setBinding(binding);
		populateSteps.add(nu);
		
		if (gridDocument != null) { // should always be
			Module gridModule = customer.getModule(gridDocument.getOwningModuleName());
			GenerateViewVisitor gridVisitor = new GenerateViewVisitor(user, customer, gridModule, gridDocument, currentUxUi, breadcrumbs);
			gridVisitor.visit();
			populateSteps.addAll(gridVisitor.populateSteps);

			populateSteps.add(new ZoomOut());
		}
	}
	
	/**
	 * Records a toolbar Save, declared or expanded from {@code <defaults/>}, of a persistent document when the user can
	 * create or update the document and the action is in the action panel and can be used; or adds a comment to the
	 * populate steps when that depends on a condition whose value is not known.
	 *
	 * @param action the declaring action metadata, which may be the {@code <defaults/>} action
	 */
	@Override
	public void visitSaveAction(@Nonnull ActionImpl action) {
		if (document.isPersistable() &&
				(! Boolean.FALSE.equals(action.getInActionPanel())) &&
				usable(action, ImplicitActionName.Save, Known.USABLE, "save", populateSteps)) {
			hasSave = true;
		}
	}
	
	@Override
	public void visitOKAction(@Nonnull ActionImpl action) {
		hasOk = true;
	}

	@Override
	public void visitCancelAction(@Nonnull ActionImpl action) {
		hasCancel = true;
	}

	/**
	 * Records a toolbar Delete, declared or expanded from {@code <defaults/>}, of a persistent document when the user
	 * can delete the document and the action is in the action panel and can be used; or keeps a comment for
	 * {@link #getDeleteComments()} when that depends on a condition whose value is not known.
	 *
	 * @param action the declaring action metadata, which may be the {@code <defaults/>} action
	 */
	@Override
	public void visitDeleteAction(@Nonnull ActionImpl action) {
		if (document.isPersistable() &&
				(! Boolean.FALSE.equals(action.getInActionPanel())) &&
				usable(action, ImplicitActionName.Delete, Known.USABLE, "delete", deleteComments)) {
			hasDelete = true;
		}
	}
	
	/**
	 * Adds a toolbar action to the action steps, when it is in the action panel and can be used.
	 */
	@Override
	public void visitCustomAction(@Nonnull ActionImpl action) {
		if (! Boolean.FALSE.equals(action.getInActionPanel())) {
			addAction(action, Known.USABLE);
		}
	}
	
	/**
	 * Adds the button's action to the action steps when the button can be used.
	 *
	 * @throws MetaDataException if the view has no action of the button's action name
	 */
	@Override
	public void visitButton(@Nonnull Button button, boolean parentVisible, boolean parentEnabled) {
		String actionName = button.getActionName();
		ActionImpl action = (ActionImpl) view.getAction(actionName);
		if (action == null) { // validated when the view is loaded
			throw new MetaDataException("Button action " + actionName + " does not exist in view " + view.getName() + " of document " + document.getName());
		}
		addAction(action, enclosing());
	}
	
	/**
	 * Adds an action step when the user's privileges let the view show the action and it can be used where it is, or
	 * a comment when that depends on a condition whose value is not known.
	 *
	 * @param action the action, custom or built-in
	 * @param enclosing what is known of the containers of the action's trigger
	 */
	private void addAction(@Nonnull ActionImpl action, @Nonnull Availability enclosing) {
		if (! usable(action, action.getImplicitName(), enclosing, "action " + action.getName(), actionSteps)) {
			return;
		}
		Action step = new Action();
		step.setActionName(action.getName());
		if (action.getConfirmationText() != null) {
			step.setConfirm(Boolean.TRUE);
		}
		actionSteps.add(step);
	}

	/**
	 * Decides whether an action's step can be generated: the user's privileges let the view show it and neither the
	 * action's conditions nor its containers' hide or disable it. When a condition whose value is not known could,
	 * adds a comment saying so instead.
	 *
	 * @param action the action
	 * @param implicitName the built-in action it stands for, or {@code null} for a custom action
	 * @param enclosing what is known of the containers of the action's trigger
	 * @param step the step, as the comment names it
	 * @param comments the steps to add the comment to
	 * @return {@code true} if the step can be generated
	 */
	private boolean usable(@Nonnull ActionImpl action,
							@Nullable ImplicitActionName implicitName,
							@Nonnull Availability enclosing,
							@Nonnull String step,
							@Nonnull List<Step> comments) {
		if (! ViewActionPrivileges.permitted(user, document, implicitName, action)) {
			return false;
		}
		Availability availability = enclosing.and(availability(action));
		if (availability.unusable()) {
			return false;
		}
		if (availability.condition() != null) {
			comments.add(notGenerated(step, availability));
			return false;
		}
		return true;
	}

	/**
	 * Returns what is known of the containers being visited, evaluating their conditions, outermost first, until one is
	 * known to hide or disable what they hold.
	 *
	 * @return the first container known to be unusable, else the first whose condition is not known, else usable
	 */
	private @Nonnull Availability enclosing() {
		Availability result = Known.USABLE;
		Iterator<Object> outermostFirst = containers.descendingIterator();
		while (outermostFirst.hasNext() && (! result.unusable())) {
			result = result.and(availability(outermostFirst.next()));
		}
		return result;
	}

	/**
	 * Returns what is known of whether metadata's own invisible and disabled conditions let it be used.
	 *
	 * @param metadata the metadata, which is {@link Invisible}, {@link Disableable}, both or neither
	 * @return what is known of the metadata
	 */
	private @Nonnull Availability availability(@Nonnull Object metadata) {
		Availability result = Known.USABLE;
		if (metadata instanceof Invisible invisible) {
			result = negative(invisible.getInvisibleCondition(),
								authored("visible",
											invisible.getXmlVisibleCondition(),
											"invisible",
											invisible.getXmlInvisibleCondition()));
		}
		if ((! result.unusable()) && (metadata instanceof Disableable disableable)) {
			result = result.and(negative(disableable.getDisabledCondition(),
											authored("enabled",
														disableable.getXmlEnabledCondition(),
														"disabled",
														disableable.getXmlDisabledCondition())));
		}
		return result;
	}

	/**
	 * Returns a condition as the view declares it, such as {@code visible="persisted"}, for a comment to name.
	 *
	 * @param positiveAttribute the attribute of the positive sense, such as {@code visible}
	 * @param positive the condition of the positive sense as declared, or {@code null}
	 * @param negativeAttribute the attribute of the negative sense, such as {@code invisible}
	 * @param negative the condition of the negative sense as declared, or {@code null}
	 * @return the declaration, or {@code null} when neither sense is declared
	 */
	private static @Nullable String authored(@Nonnull String positiveAttribute,
												@Nullable String positive,
												@Nonnull String negativeAttribute,
												@Nullable String negative) {
		if (positive != null) {
			return positiveAttribute + "=\"" + positive + '"';
		}
		return (negative == null) ? null : negativeAttribute + "=\"" + negative + '"';
	}

	/**
	 * Returns what is known of a negative (invisible or disabled) condition, evaluating it once per condition.
	 *
	 * @param condition the condition, or {@code null} when none was declared
	 * @param declaration the condition as the view declares it, which a comment names, or {@code null} to name
	 * 			{@code condition}
	 * @return usable when there is no condition or it is known to be false, unusable when it is known to be true, or
	 * 			usable unless the condition, whose value is not known
	 */
	private @Nonnull Availability negative(@Nullable String condition, @Nullable String declaration) {
		if (condition == null) {
			return Known.USABLE;
		}
		Availability result = conditions.computeIfAbsent(condition, this::evaluateNegative);
		if ((result.condition() == null) || (declaration == null)) {
			return result;
		}
		return new Availability(false, declaration, result.reason());
	}

	/**
	 * Evaluates a negative condition by the rules in the class description.
	 *
	 * <p>Side effects: the first evaluation creates the instances, see {@link #instances()}.
	 *
	 * @param condition the condition
	 * @return what is known of the condition
	 */
	private @Nonnull Availability evaluateNegative(@Nonnull String condition) {
		if (String.valueOf(true).equals(condition)) {
			return Known.UNUSABLE;
		}
		if (String.valueOf(false).equals(condition)) {
			return Known.USABLE;
		}
		if (SCRIPT_STATE_CONDITIONS.contains(condition)) {
			return new Availability(false, condition, null);
		}
		boolean whenOpened;
		boolean whenEntered;
		try {
			Instances i = instances();
			whenOpened = i.opened().evaluateCondition(condition);
			whenEntered = i.entered().evaluateCondition(condition);
		}
		catch (Exception e) { // the step is left out with the reason, rather than failing the whole script
			LOGGER.debug("Condition {} of {}.{} cannot be evaluated for SAIL generation", condition, module.getName(), document.getName(), e);
			return new Availability(false, condition, reason(e));
		}
		if (whenOpened != whenEntered) {
			return new Availability(false, condition, null);
		}
		return whenOpened ? Known.UNUSABLE : Known.USABLE;
	}

	/**
	 * Returns the instances that conditions are evaluated against, creating them the first time.
	 *
	 * <p>Side effects: the first call creates a new instance as the user (running the document's Bizlet
	 * {@code newInstance}) and builds a random instance with the {@code sail} fixture; neither is saved. When that
	 * fails, the failure is kept and every later call fails with its reason, without trying again.
	 *
	 * @return the instances
	 * @throws IllegalStateException if the instances cannot be created, now or on an earlier call
	 */
	private @Nonnull Instances instances() {
		Instances result = instances;
		if (result == null) {
			if (instancesFailure != null) {
				throw new IllegalStateException(instancesFailure);
			}
			try {
				Bean opened = document.newInstance(user);
				Bean entered = new DataBuilder().fixture(FixtureType.sail).build(document);
				result = new Instances(opened, entered);
			}
			catch (Exception e) {
				instancesFailure = reason(e);
				throw new IllegalStateException(instancesFailure, e);
			}
			instances = result;
		}
		return result;
	}

	/**
	 * Returns why something could not be evaluated, for a comment.
	 *
	 * @param e what was thrown
	 * @return its message, or a fixed reason naming its type when it has none
	 */
	private static @Nonnull String reason(@Nonnull Exception e) {
		String result = e.getMessage();
		return (result == null) ? e.getClass().getSimpleName() + " without a message" : result;
	}

	/**
	 * Returns a comment that stands in for a step that is not generated.
	 *
	 * @param step the step, such as {@code action Restore}
	 * @param availability what is known of the step, whose condition is not known
	 * @return the comment
	 */
	private static @Nonnull Comment notGenerated(@Nonnull String step, @Nonnull Availability availability) {
		StringBuilder comment = new StringBuilder(128).append("not generated: ").append(step);
		comment.append(" depends on ").append(availability.condition());
		String reason = availability.reason();
		if (reason != null) {
			comment.append(", which cannot be evaluated here (").append(reason).append(')');
		}
		Comment result = new Comment();
		result.setComment(comment.toString());
		return result;
	}
	
	@Nonnull List<Step> getPopulateSteps() {
		return populateSteps;
	}
	
	@Nonnull List<Step> getActionSteps() {
		return actionSteps;
	}

	/**
	 * Returns the comments that stand in for Delete actions that are not generated, for a generator that would delete
	 * when {@link #getHasDelete()} is {@code false}.
	 *
	 * @return the comments, empty when there are none
	 */
	@Nonnull List<Step> getDeleteComments() {
		return deleteComments;
	}
	
	boolean getHasSave() {
		return hasSave;
	}
	
	boolean getHasOk() {
		return hasOk && document.isPersistable();
	}
	
	boolean getHasCancel() {
		return hasCancel;
	}
	
	boolean getHasDelete() {
		return hasDelete;
	}

	/**
	 * The instances that conditions are evaluated against.
	 *
	 * @param opened the new instance that the script opens
	 * @param entered an instance holding test data, such as the script enters
	 */
	private record Instances(@Nonnull Bean opened, @Nonnull Bean entered) {
	}

	/**
	 * What is known of whether a step's widget can be used.
	 *
	 * @param unusable whether a known condition hides or disables it
	 * @param condition when it is not unusable, a condition whose value is not known and that could hide or disable
	 * 			it, or {@code null} when none could
	 * @param reason why that condition could not be evaluated, or {@code null} when it was, or none could
	 */
	private record Availability(boolean unusable, @Nullable String condition, @Nullable String reason) {
		/**
		 * Returns what is known of a widget that both this and {@code other} apply to.
		 *
		 * @param other what else applies; must not be {@code null}
		 * @return the first unusable, else the first whose condition is not known, else usable
		 */
		@Nonnull Availability and(@Nonnull Availability other) {
			if (unusable || ((condition != null) && (! other.unusable))) {
				return this;
			}
			return other;
		}
	}

	/**
	 * The availabilities that name no condition.
	 */
	private static final class Known {
		/** Nothing hides or disables the widget. */
		static final Availability USABLE = new Availability(false, null, null);
		/** A known condition hides or disables the widget. */
		static final Availability UNUSABLE = new Availability(true, null, null);

		private Known() {
			// constants only
		}
	}
}
