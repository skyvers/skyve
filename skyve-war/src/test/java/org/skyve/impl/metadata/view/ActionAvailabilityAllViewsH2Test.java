package org.skyve.impl.metadata.view;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.ActionUnavailableException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.router.UxUiMetadata;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.metadata.view.event.Addable;
import org.skyve.impl.metadata.view.event.Changeable;
import org.skyve.impl.metadata.view.event.Editable;
import org.skyve.impl.metadata.view.event.EventSource;
import org.skyve.impl.metadata.view.event.Focusable;
import org.skyve.impl.metadata.view.event.Removable;
import org.skyve.impl.metadata.view.event.Selectable;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;
import org.skyve.impl.metadata.view.reference.ActionReference;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.widget.bound.Bound;
import org.skyve.util.Binder.TargetMetaData;

import modules.admin.DataMaintenance.actions.CheckContent;
import modules.admin.domain.DataMaintenance;
import modules.test.AbstractSkyveTestDispose;

/**
 * Runs {@link ActionAvailability} over every edit and create view of every customer in the repository, for every
 * UX/UI, and writes the actions that its enforcement could start refusing to
 * {@code target/action-availability-report.txt} (relative to the module directory surefire runs in).
 *
 * <p>Each check runs under an all-false and an all-true truth table; the report header explains the policy.
 *
 * <p>The test fails when a check throws anything but {@link ActionUnavailableException}, when no view, row link or
 * pass under all true was checked, when a check passes under all true but is refused under all false (which would
 * make the report's "no trigger under any assignment" unsound), or when the UX/UIs in which DataMaintenance
 * CheckContent has no trigger are other than exactly the non-desktop ones.
 * The report's contents never fail it: it is the pre-release list of risky views, and its lines are keyed by module
 * and view with the customers and UX/UIs they hold for, so it reads the same over another application's customers.
 */
class ActionAvailabilityAllViewsH2Test extends AbstractSkyveTestDispose {
	private static final Path REPORT = Path.of("target", "action-availability-report.txt");

	private static final Bean ALL_FALSE = truthTableBean(false);
	private static final Bean ALL_TRUE = truthTableBean(true);

	/** The built-in actions without a class that {@link ActionAvailability} decides on, which {@code <defaults/>} stands for too. */
	private static final Set<ImplicitActionName> BUILT_IN = EnumSet.of(ImplicitActionName.OK, ImplicitActionName.Save, ImplicitActionName.Delete);
	/** The built-in actions with a class, which are requested by class like a custom action. */
	private static final Set<ImplicitActionName> CLASS_BASED = EnumSet.of(ImplicitActionName.BizExport,
																			ImplicitActionName.BizImport,
																			ImplicitActionName.Download,
																			ImplicitActionName.Upload);
	/** The built-in actions that {@link ActionAvailability} decides on; the others are checked for exceptions only. */
	private static final Set<ImplicitActionName> IN_SCOPE = union(BUILT_IN, CLASS_BASED);
	/** The built-in action names that SmartClientEditServlet treats as built-in when a request names them in _a. */
	private static final Set<String> BUILT_IN_NAMES = Set.of(ImplicitActionName.OK.name(),
																ImplicitActionName.Save.name(),
																ImplicitActionName.ZoomOut.name(),
																ImplicitActionName.Print.name());

	private static final String EVENT_ONLY = "1. Actions reachable only through a <server> event handler";
	private static final String SHARED_CLASS = "2. Action classes shared by several named actions in one view";
	private static final String BUILT_IN_COLLISION = "2b. Custom or class-based actions whose class name equals a built-in action name";
	private static final String NEVER_REACHABLE = "3. Actions with no trigger under any assignment";
	private static final String ROW_LINK_REFUSED = "3b. Grid-row links refused under all false";
	private static final String REFRESH_ACTION = "4a. Views with a refreshAction (Backward compatibility item 4)";
	private static final String REVEALED = "4b. <server> handlers on widgets revealed by <toggleVisibility> or <setInvisible> (Backward compatibility item 5)";
	private static final String OTHER_REVEALS = "4c. Other <toggleVisibility> and <setInvisible> uses, whose target has no <server> handler";

	/** The gating assertions: one per check that threw anything but a refusal, then those over the whole run. */
	private final List<Executable> failures = new ArrayList<>();
	/** The checks that pass under all true but are refused under all false, by case name. */
	private final List<String> anomalies = new ArrayList<>();
	/** The in-scope actions checked, for the report. */
	private final List<ActionResult> results = new ArrayList<>();
	/** Row-link targets, keyed by {@link #rowLinkTarget}, to where the links are. */
	private final Map<String, SortedSet<String>> rowLinkTargets = new TreeMap<>();
	/** The row-link targets, keyed by {@link #rowLinkTarget}, of which some checkRow is refused under all false. */
	private final Set<String> refusedRowLinkTargets = new TreeSet<>();
	/** Section title to line to customer to the UX/UIs the line holds for. */
	private final Map<String, SortedMap<String, SortedMap<String, SortedSet<String>>>> report = new LinkedHashMap<>();
	private int viewCount = 0;
	private int checkCount = 0;
	private int rowCheckCount = 0;
	private int allTruePassCount = 0;

	private enum Trigger {
		TOOLBAR, BUTTON, LINK, EVENT, ROW_LINK;

		String label() {
			return name().toLowerCase(Locale.ROOT).replace('_', ' ');
		}
	}

	private record RowLink(String gridBinding, String actionName) {
	}

	private record Reveal(String eventAction, String targetBinding, String source) {
	}

	private record Outcome(boolean passesAllFalse, boolean passesAllTrue) {
	}

	private record ActionResult(String customerName,
									String uxui,
									String documentName,
									String where,
									Action action,
									String label,
									Set<Trigger> triggers,
									Outcome outcome) {
	}

	@Test
	void everyViewChecksEveryActionWithoutUnexpectedExceptions() throws IOException {
		for (String section : List.of(EVENT_ONLY, SHARED_CLASS, BUILT_IN_COLLISION, NEVER_REACHABLE, ROW_LINK_REFUSED, REFRESH_ACTION, REVEALED, OTHER_REVEALS)) {
			report.put(section, new TreeMap<>());
		}
		ProvidedRepository repository = ProvidedRepositoryFactory.get();
		Router router = repository.getRouter();
		if (router == null) {
			throw new IllegalStateException("The repository has no router");
		}
		List<String> uxuis = new ArrayList<>();
		for (UxUiMetadata uxui : router.getUxUis()) {
			uxuis.add(uxui.getName());
		}
		uxuis.sort(null);
		List<String> customerNames = new ArrayList<>(repository.getAllCustomerNames());
		customerNames.sort(null);
		for (String customerName : customerNames) {
			SuperUser user = new SuperUser();
			user.setCustomerName(customerName);
			user.setName(u.getName());
			CustomerImpl customer = (CustomerImpl) user.getCustomer();
			for (Module module : customer.getModules()) {
				for (String documentName : module.getDocumentRefs().keySet()) {
					Document document = module.getDocument(customer, documentName);
					// a document referenced from another module is checked with its own module
					if (module.getName().equals(document.getOwningModuleName())) {
						for (String uxui : uxuis) {
							for (ViewType viewType : List.of(ViewType.edit, ViewType.create)) {
								// a missing create view falls back to the edit view at run time, which is checked anyway
								View view = repository.getView(uxui, customer, document, viewType.toString());
								if (view != null) {
									checkView(repository, customer, user, document, view, uxui);
								}
							}
						}
					}
				}
			}
		}
		writeReport(customerNames, uxuis);

		int views = viewCount;
		int checks = checkCount;
		int rowChecks = rowCheckCount;
		int allTruePasses = allTruePassCount;
		failures.add(() -> assertTrue((views > 0) && (checks > 0) && (rowChecks > 0) && (allTruePasses > 0),
										"Too little was checked: " + views + " views, " + checks + " checks, " + rowChecks + " row checks, " +
											allTruePasses + " passes under all true"));
		failures.add(() -> assertTrue(anomalies.isEmpty(), "Passes under all true but refused under all false: " + anomalies));
		// a known case of Backward compatibility item 6: the default view declares CheckContent with no trigger
		SortedSet<String> nonDesktop = new TreeSet<>(uxuis);
		nonDesktop.remove(UxUi.DESKTOP_NAME);
		Map<String, SortedSet<String>> expectedCheckContent = new TreeMap<>();
		for (String customerName : customerNames) {
			expectedCheckContent.put(customerName, nonDesktop);
		}
		String checkContentLine = DataMaintenance.MODULE_NAME + '.' + DataMaintenance.DOCUMENT_NAME + ' ' + ViewType.edit + ": " +
									CheckContent.class.getSimpleName() + ' ';
		Map<String, SortedSet<String>> checkContent = new TreeMap<>();
		for (Entry<String, SortedMap<String, SortedSet<String>>> line : report.get(NEVER_REACHABLE).entrySet()) {
			if (line.getKey().startsWith(checkContentLine)) {
				for (Entry<String, SortedSet<String>> where : line.getValue().entrySet()) {
					checkContent.computeIfAbsent(where.getKey(), k -> new TreeSet<>()).addAll(where.getValue());
				}
			}
		}
		failures.add(() -> assertEquals(expectedCheckContent, checkContent, "DataMaintenance CheckContent is unreachable where"));
		assertAll(failures.stream());
	}

	/**
	 * Checks every action the view declares and every row link in its data widgets under both truth tables, and
	 * records the view's report lines.
	 */
	private void checkView(ProvidedRepository repository, CustomerImpl customer, User user, Document document, View view, String uxui) {
		viewCount++;
		String customerName = customer.getName();
		String documentName = document.getOwningModuleName() + '.' + document.getName();
		String where = documentName + ' ' + view.getName();
		String caseName = customerName + ' ' + uxui + ' ' + where + ' ';
		TriggerRecorder recorder = new TriggerRecorder(customer, document, (ViewImpl) view, uxui);
		recorder.visit();

		Map<String, SortedSet<String>> namesByClass = new TreeMap<>();
		for (Action action : view.getActions()) {
			ImplicitActionName implicitName = action.getImplicitName();
			String name = action.getName();
			String resourceName = action.getResourceName();
			if (implicitName == ImplicitActionName.DEFAULTS) {
				for (ImplicitActionName expanded : BUILT_IN) {
					Outcome outcome = check(caseName + expanded + " (<defaults/>)",
												b -> ActionAvailability.check(user, document, view, uxui, b, expanded));
					addResult(customerName, uxui, documentName, where, action, expanded + " (<defaults/>)", recorder.triggers(builtInKey(expanded)), outcome);
				}
				continue;
			}

			// a request names an action with a class by its class, and a built-in without one by its implicit name;
			// the implicit check of an action with a class passes for any reachable action of its type, so it only gates
			Outcome outcome = null;
			if (implicitName != null) {
				outcome = check(caseName + implicitName, b -> ActionAvailability.check(user, document, view, uxui, b, implicitName));
			}
			if (resourceName != null) {
				outcome = check(caseName + name + " by class " + resourceName,
									b -> ActionAvailability.check(user, document, view, uxui, b, resourceName));
				if ((implicitName == null) || CLASS_BASED.contains(implicitName)) {
					namesByClass.computeIfAbsent(resourceName, k -> new TreeSet<>()).add(name);
					if (BUILT_IN_NAMES.contains(resourceName)) {
						add(BUILT_IN_COLLISION, where + ": action " + name + " (class " + resourceName + ')', customerName, uxui);
					}
				}
			}
			if ((implicitName == null) || IN_SCOPE.contains(implicitName)) {
				addResult(customerName, uxui, documentName, where, action, name, recorder.triggers(TriggerRecorder.key(action)), outcome);
			}
		}

		for (Entry<String, SortedSet<String>> entry : namesByClass.entrySet()) {
			if (entry.getValue().size() > 1) {
				add(SHARED_CLASS, where + ": class " + entry.getKey() + " is shared by " + String.join(", ", entry.getValue()), customerName, uxui);
			}
		}

		String refreshActionName = view.getRefreshActionName();
		if (refreshActionName != null) {
			Action refreshAction = view.getAction(refreshActionName);
			String triggers = (refreshAction == null) ? "undeclared" : labels(recorder.triggers(TriggerRecorder.key(refreshAction)));
			add(REFRESH_ACTION,
					where + ": refreshAction " + refreshActionName + " every " + view.getRefreshTimeInSeconds() + "s; its triggers: " + triggers,
					customerName,
					uxui);
		}

		for (Reveal reveal : recorder.reveals) {
			String line = where + ": <" + reveal.eventAction() + " binding=\"" + reveal.targetBinding() + "\"> on " + reveal.source();
			SortedSet<String> targetActions = recorder.serverActionsBySource.get(reveal.targetBinding());
			if (targetActions == null) {
				add(OTHER_REVEALS, line, customerName, uxui);
			}
			else {
				add(REVEALED, line + " reveals a widget whose <server> handlers target " + String.join(", ", targetActions), customerName, uxui);
			}
		}

		for (RowLink rowLink : recorder.rowLinks) {
			checkRowLink(repository, customer, user, document, view, uxui, caseName, where, rowLink);
		}
	}

	/**
	 * Checks a row link against each edit and create view of its row document, and records it as a trigger of the
	 * row document's action.
	 */
	private void checkRowLink(ProvidedRepository repository,
								CustomerImpl customer,
								User user,
								Document formDocument,
								View formView,
								String uxui,
								String caseName,
								String where,
								RowLink rowLink) {
		String gridBinding = rowLink.gridBinding();
		String actionName = rowLink.actionName();
		Module formModule = customer.getModule(formDocument.getOwningModuleName());
		TargetMetaData target = BindUtil.getMetaDataForBinding(customer, formModule, formDocument, gridBinding);
		Attribute relation = target.getAttribute();
		if (relation == null) {
			throw new IllegalStateException(where + " grid " + gridBinding + " is not bound to a relation");
		}
		Document rowDocument = target.getDocument().getRelatedDocument(customer, relation.getName());
		String rowDocumentName = rowDocument.getOwningModuleName() + '.' + rowDocument.getName();
		String customerName = customer.getName();
		String rowLinkTarget = rowLinkTarget(customerName, uxui, rowDocumentName, actionName);
		rowLinkTargets.computeIfAbsent(rowLinkTarget, k -> new TreeSet<>()).add(where + " grid " + gridBinding);
		for (ViewType viewType : List.of(ViewType.edit, ViewType.create)) {
			View rowView = repository.getView(uxui, customer, rowDocument, viewType.toString());
			if (rowView != null) {
				rowCheckCount++;
				String row = "grid " + gridBinding + " row " + rowDocumentName + ' ' + rowView.getName() + ": " + actionName;
				Outcome outcome = check(caseName + row,
											b -> ActionAvailability.checkRow(user, formDocument, formView, uxui, b, gridBinding, rowDocument, rowView, b, actionName));
				if (! outcome.passesAllFalse()) {
					refusedRowLinkTargets.add(rowLinkTarget);
					add(ROW_LINK_REFUSED, where + ' ' + row, customerName, uxui);
				}
			}
		}
	}

	private void addResult(String customerName,
							String uxui,
							String documentName,
							String where,
							Action action,
							String label,
							Set<Trigger> triggers,
							Outcome outcome) {
		results.add(new ActionResult(customerName, uxui, documentName, where, action, label, EnumSet.copyOf(triggers), outcome));
	}

	/** Runs a check under both truth tables, recording unexpected exceptions and a pass under all true only. */
	private Outcome check(String caseName, Consumer<Bean> check) {
		checkCount++;
		Outcome result = new Outcome(passes(caseName + " under all false", ALL_FALSE, check),
										passes(caseName + " under all true", ALL_TRUE, check));
		if (result.passesAllTrue()) {
			allTruePassCount++;
			if (! result.passesAllFalse()) {
				anomalies.add(caseName);
			}
		}
		return result;
	}

	private boolean passes(String caseName, Bean bean, Consumer<Bean> check) {
		try {
			check.accept(bean);
			return true;
		}
		catch (@SuppressWarnings("unused") ActionUnavailableException e) {
			return false;
		}
		catch (RuntimeException e) {
			failures.add(() -> fail(caseName + " threw " + e, e));
			return false;
		}
	}

	/**
	 * Returns a bean that answers every condition with {@code value}, except the literals {@code true} and
	 * {@code false}, and refuses every other method but {@link Bean#getBizId()}, which the refusal warning logs.
	 */
	private static Bean truthTableBean(boolean value) {
		String table = "all " + value;
		return (Bean) Proxy.newProxyInstance(Bean.class.getClassLoader(), new Class<?>[] {Bean.class}, (proxy, method, args) -> {
			String methodName = method.getName();
			if ("evaluateCondition".equals(methodName)) {
				String condition = (String) args[0];
				if (String.valueOf(true).equals(condition) || String.valueOf(false).equals(condition)) {
					return Boolean.valueOf(condition);
				}
				return Boolean.valueOf(value);
			}
			if ("getBizId".equals(methodName)) {
				return table;
			}
			if ("toString".equals(methodName)) {
				return table + " truth table bean";
			}
			throw new UnsupportedOperationException("ActionAvailability called " + methodName + " on the " + table + " truth table bean");
		});
	}

	/**
	 * Files each in-scope action into the report sections, now that every row link is known, and writes the report.
	 */
	private void writeReport(List<String> customerNames, List<String> uxuis) throws IOException {
		for (ActionResult result : results) {
			Action action = result.action();
			Set<Trigger> triggers = result.triggers();
			// checkRow resolves a link's action in the row view by the name the link gives, so only that action is linked
			String rowLinkTarget = rowLinkTarget(result, action.getName());
			SortedSet<String> rowLinksFrom = rowLinkTargets.getOrDefault(rowLinkTarget, Collections.emptySortedSet());
			if (! rowLinksFrom.isEmpty()) {
				triggers.add(Trigger.ROW_LINK);
			}
			// the form-level check refuses an action reached by row links, but it is reachable when checkRow passes for every one
			boolean reachableByRowLinks = (! rowLinksFrom.isEmpty()) && (! refusedRowLinkTargets.contains(rowLinkTarget));

			String line = result.where() + ": " + result.label();
			Outcome outcome = result.outcome();
			if (triggers.equals(EnumSet.of(Trigger.EVENT))) {
				add(EVENT_ONLY, line + " (" + describe(outcome) + ')', result.customerName(), result.uxui());
			}
			if ((! outcome.passesAllFalse()) && (! reachableByRowLinks)) {
				StringBuilder detail = new StringBuilder(line).append(" (triggers: ").append(labels(triggers));
				if (! rowLinksFrom.isEmpty()) {
					detail.append(", row links from ").append(String.join(", ", rowLinksFrom));
				}
				if (Boolean.FALSE.equals(action.getInActionPanel())) {
					detail.append("; inActionPanel=false");
				}
				appendCondition(detail, "invisible", action.getInvisibleCondition());
				appendCondition(detail, "disabled", action.getDisabledCondition());
				add(NEVER_REACHABLE, detail.append(')').toString(), result.customerName(), result.uxui());
			}
		}

		StringBuilder text = new StringBuilder(8192);
		text.append("Action availability report\n");
		text.append("Generated by ActionAvailabilityAllViewsH2Test over every edit and create view of every customer, for every UX/UI.\n");
		text.append("This report never fails the build.\n\n");
		text.append("Customers: ").append(String.join(", ", customerNames)).append(". UX/UIs: ").append(String.join(", ", uxuis)).append(".\n");
		text.append("Checked: ").append(viewCount).append(" views (customer, UX/UI and view combinations), ");
		text.append(checkCount - rowCheckCount).append(" action checks and ").append(rowCheckCount).append(" row-link checks, each under both truth tables.\n\n");
		text.append("""
				Truth tables: ActionAvailability hands each view condition string to Bean.evaluateCondition unevaluated, so the
				table bean answers every condition, whether a name, its "not" form or an inline {el:...}, {role:...} or other
				expression, with the table's value; only the literals true and false are taken as written. Every rule needs the
				invisible and disabled conditions on a trigger's path to be false, so all false is the most permissive assignment
				(a superset of what any real bean allows, as a condition and its "not" form are not tied together) and all true
				the least permissive.

				Scope: custom actions, the class-based BizExport, BizImport, Download and Upload, and the built-in OK, Save and
				Delete, including those <defaults/> stands for (decision 7). Other built-ins are checked for exceptions only.
				Triggers are what the metadata declares, whatever its conditions: toolbar (inActionPanel not false), button, link
				(outside data widgets), event (<server> handler) and row link (a link in another view's data grid or repeater, to
				this action of the row document's view, which ActionAvailability.checkRow decides rather than check).
				Each line ends with [customers: UX/UIs] where it holds; "all" means every UX/UI.

				Definitions:
				1. Triggers are exactly {event}. The outcome under each table is shown.
				2. Two or more custom or class-based actions in one view with the same class; a request by class name passes
				   when either is reachable.
				2b. A custom or class-based action whose class (resource) name is OK, Save, ZoomOut or Print. SmartClient sends
				   a custom action's class name in _a, and SmartClientEditServlet treats those four names there as the built-in
				   action.
				3. Refused by ActionAvailability.check under the all-false table. As that table is the most permissive, the
				   action is refused for every bean state; the test fails if any check passes under all true but is refused
				   under all false. Each line lists the action's triggers in the metadata ("none" when it has none) and its own
				   inActionPanel, invisible and disabled settings. An action with a class is checked by class. An action with
				   row links (whose form-level check is always refused) is left out when every checkRow for its links passes
				   under all false; otherwise it is listed with the links, and the refused ones are in 3b.
				3b. A link in a data grid or repeater row, checked with ActionAvailability.checkRow against each view of the
				   row document, refused under the all-false table: form view, grid binding, row document and view, action.
				4a. A view's refreshAction is not a trigger, so a client that sends it is refused unless another trigger is
				   reachable.
				4b. The target of a <setInvisible> or <toggleVisibility> event action, matched by binding, has <server> handlers.
				   If the target's invisible condition is true on the server, revealing it in the browser does not make those
				   handlers reachable.
				4c. Every other <toggleVisibility> or <setInvisible> event action, by view, target binding and source widget.

				""");
		text.append("Section counts:\n");
		for (Entry<String, SortedMap<String, SortedMap<String, SortedSet<String>>>> section : report.entrySet()) {
			text.append("  ").append(section.getKey()).append(": ").append(section.getValue().size()).append('\n');
		}
		for (Entry<String, SortedMap<String, SortedMap<String, SortedSet<String>>>> section : report.entrySet()) {
			text.append('\n').append(section.getKey()).append(" (").append(section.getValue().size()).append(")\n");
			for (Entry<String, SortedMap<String, SortedSet<String>>> line : section.getValue().entrySet()) {
				text.append("  ").append(line.getKey()).append(' ').append(locations(line.getValue(), uxuis)).append('\n');
			}
		}
		Files.createDirectories(REPORT.getParent());
		Files.writeString(REPORT, text);
	}

	private void add(String section, String line, String customerName, String uxui) {
		report.get(section)
				.computeIfAbsent(line, k -> new TreeMap<>())
				.computeIfAbsent(customerName, k -> new TreeSet<>())
				.add(uxui);
	}

	/** Renders where a line holds: [c1, c2: u1, u2] when every customer has the same UX/UIs, else [c1: u1; c2: u2]. */
	private static String locations(SortedMap<String, SortedSet<String>> uxuisByCustomer, List<String> allUxUis) {
		Collection<SortedSet<String>> uxuiSets = uxuisByCustomer.values();
		if (uxuiSets.stream().distinct().count() == 1) {
			return '[' + String.join(", ", uxuisByCustomer.keySet()) + ": " + uxuis(uxuiSets.iterator().next(), allUxUis) + ']';
		}
		return uxuisByCustomer.entrySet()
								.stream()
								.map(e -> e.getKey() + ": " + uxuis(e.getValue(), allUxUis))
								.collect(Collectors.joining("; ", "[", "]"));
	}

	private static String uxuis(SortedSet<String> uxuis, List<String> allUxUis) {
		return (uxuis.size() == allUxUis.size()) ? "all" : String.join(", ", uxuis);
	}

	private static String labels(Set<Trigger> triggers) {
		if (triggers.isEmpty()) {
			return "none";
		}
		return triggers.stream().map(Trigger::label).collect(Collectors.joining(", "));
	}

	private static String describe(Outcome outcome) {
		return "all false: " + (outcome.passesAllFalse() ? "passes" : "refused") +
				", all true: " + (outcome.passesAllTrue() ? "passes" : "refused");
	}

	private static void appendCondition(StringBuilder detail, String attribute, String condition) {
		if (condition != null) {
			detail.append("; ").append(attribute).append('=').append(condition);
		}
	}

	private static Set<ImplicitActionName> union(Set<ImplicitActionName> first, Set<ImplicitActionName> second) {
		Set<ImplicitActionName> result = EnumSet.copyOf(first);
		result.addAll(second);
		return result;
	}

	/** Keys a built-in action apart from a custom action that has its name, such as a custom action named Save. */
	private static String builtInKey(ImplicitActionName implicitName) {
		return '<' + implicitName.name() + '>';
	}

	/** Keys a row-link target by customer, UX/UI, row document ("module.Document") and the action name the link gives. */
	private static String rowLinkTarget(String customerName, String uxui, String documentName, String actionName) {
		return String.join("|", customerName, uxui, documentName, actionName);
	}

	private static String rowLinkTarget(ActionResult result, String actionName) {
		return rowLinkTarget(result.customerName(), result.uxui(), result.documentName(), actionName);
	}

	/**
	 * Records, by action, the kinds of trigger the view declares for it whatever their conditions, plus the row links,
	 * the {@code <server>} handlers by widget binding, and the event actions that reveal a widget.
	 *
	 * <p>OK, Save and Delete (declared or from {@code <defaults/>}) are keyed by {@link #builtInKey}, and every
	 * other action by its name, as {@link #key(Action)} keys the action a widget references.
	 */
	private static final class TriggerRecorder extends NoOpViewVisitor {
		private final Map<String, Set<Trigger>> triggersByAction = new TreeMap<>();
		private final List<RowLink> rowLinks = new ArrayList<>();
		private final List<Reveal> reveals = new ArrayList<>();
		private final Map<String, SortedSet<String>> serverActionsBySource = new TreeMap<>();
		/** The binding of the data grid or repeater being visited, or {@code null} outside one. */
		private String dataWidgetBinding;
		/** The widget whose event handler was entered last; handlers do not nest. */
		private EventSource eventSource;

		TriggerRecorder(CustomerImpl customer, Document document, ViewImpl view, String uxui) {
			super(customer, (ModuleImpl) customer.getModule(document.getOwningModuleName()), (DocumentImpl) document, view, uxui);
		}

		Set<Trigger> triggers(String key) {
			return triggersByAction.getOrDefault(key, EnumSet.noneOf(Trigger.class));
		}

		static String key(Action action) {
			ImplicitActionName implicitName = action.getImplicitName();
			if (BUILT_IN.contains(implicitName)) {
				return builtInKey(implicitName);
			}
			return action.getName();
		}

		private void addTrigger(String key, Trigger trigger) {
			triggersByAction.computeIfAbsent(key, k -> EnumSet.noneOf(Trigger.class)).add(trigger);
		}

		private void recordReference(String actionName, Trigger trigger) {
			Action action = view.getAction(actionName);
			addTrigger((action == null) ? actionName : key(action), trigger);
		}

		private void recordToolbar(ActionImpl action, String key) {
			if (! Boolean.FALSE.equals(action.getInActionPanel())) {
				addTrigger(key, Trigger.TOOLBAR);
			}
		}

		private String source() {
			return (eventSource instanceof Bound bound) ? bound.getBinding() : eventSource.getClass().getSimpleName();
		}

		@Override
		public void visitCustomAction(ActionImpl action) {
			recordToolbar(action, action.getName());
		}

		@Override
		public void visitOKAction(ActionImpl action) {
			recordToolbar(action, builtInKey(ImplicitActionName.OK));
		}

		@Override
		public void visitSaveAction(ActionImpl action) {
			recordToolbar(action, builtInKey(ImplicitActionName.Save));
		}

		@Override
		public void visitDeleteAction(ActionImpl action) {
			recordToolbar(action, builtInKey(ImplicitActionName.Delete));
		}

		@Override
		public void visitBizExportAction(ActionImpl action) {
			recordToolbar(action, action.getName());
		}

		@Override
		public void visitBizImportAction(ActionImpl action) {
			recordToolbar(action, action.getName());
		}

		@Override
		public void visitDownloadAction(ActionImpl action) {
			recordToolbar(action, action.getName());
		}

		@Override
		public void visitUploadAction(ActionImpl action) {
			recordToolbar(action, action.getName());
		}

		@Override
		public void visitButton(Button button, boolean parentVisible, boolean parentEnabled) {
			recordReference(button.getActionName(), Trigger.BUTTON);
		}

		@Override
		public void visitLink(Link link, boolean parentVisible, boolean parentEnabled) {
			if (link.getReference() instanceof ActionReference reference) {
				if (dataWidgetBinding == null) {
					recordReference(reference.getActionName(), Trigger.LINK);
				}
				else {
					rowLinks.add(new RowLink(dataWidgetBinding, reference.getActionName()));
				}
			}
		}

		@Override
		public void visitDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
			dataWidgetBinding = grid.getBinding();
		}

		@Override
		public void visitedDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
			dataWidgetBinding = null;
		}

		@Override
		public void visitDataRepeater(DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
			dataWidgetBinding = repeater.getBinding();
		}

		@Override
		public void visitedDataRepeater(DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
			dataWidgetBinding = null;
		}

		@Override
		public void visitServerSideActionEventAction(ServerSideActionEventAction server, boolean parentVisible, boolean parentEnabled) {
			String actionName = server.getActionName();
			recordReference(actionName, Trigger.EVENT);
			serverActionsBySource.computeIfAbsent(source(), k -> new TreeSet<>()).add(actionName);
		}

		@Override
		public void visitSetInvisibleEventAction(SetInvisibleEventAction setInvisible, boolean parentVisible, boolean parentEnabled) {
			reveals.add(new Reveal("setInvisible", setInvisible.getBinding(), source()));
		}

		@Override
		public void visitToggleVisibilityEventAction(ToggleVisibilityEventAction toggleVisibility, boolean parentVisible, boolean parentEnabled) {
			reveals.add(new Reveal("toggleVisibility", toggleVisibility.getBinding(), source()));
		}

		@Override
		public void visitOnChangedEventHandler(Changeable changeable, boolean parentVisible, boolean parentEnabled) {
			eventSource = changeable;
		}

		@Override
		public void visitOnFocusEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
			eventSource = blurable;
		}

		@Override
		public void visitOnBlurEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
			eventSource = blurable;
		}

		@Override
		public void visitOnAddedEventHandler(Addable addable, boolean parentVisible, boolean parentEnabled) {
			eventSource = addable;
		}

		@Override
		public void visitOnEditedEventHandler(Editable editable, boolean parentVisible, boolean parentEnabled) {
			eventSource = editable;
		}

		@Override
		public void visitOnRemovedEventHandler(Removable removable, boolean parentVisible, boolean parentEnabled) {
			eventSource = removable;
		}

		@Override
		public void visitOnSelectedEventHandler(Selectable selectable, boolean parentVisible, boolean parentEnabled) {
			eventSource = selectable;
		}

		@Override
		public void visitOnPickedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
			eventSource = lookup;
		}

		@Override
		public void visitOnClearedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
			eventSource = lookup;
		}
	}
}
