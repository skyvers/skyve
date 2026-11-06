package org.skyve.impl.sail.recording;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.IntConsumer;
import java.util.function.ObjIntConsumer;
import java.util.function.Consumer;

import org.dom4j.Attribute;
import org.dom4j.Document;
import org.dom4j.DocumentHelper;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.Node;
import org.dom4j.QName;
import org.dom4j.io.OutputFormat;
import org.dom4j.io.XMLWriter;
import org.skyve.impl.util.SecureDom4j;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Interaction;
import org.skyve.metadata.sail.language.Procedure;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.CaptureCell;
import org.skyve.metadata.sail.language.step.CaptureTitle;
import org.skyve.metadata.sail.language.step.CaptureValue;
import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.TestGridRows;
import org.skyve.metadata.sail.language.step.TestMessage;
import org.skyve.metadata.sail.language.step.TestValue;
import org.skyve.metadata.sail.language.step.interaction.actions.AbstractAction;
import org.skyve.metadata.sail.language.step.interaction.DataEnter;
import org.skyve.metadata.sail.language.step.interaction.DataEnter.Match;
import org.skyve.metadata.sail.language.step.interaction.TabSelect;
import org.skyve.metadata.sail.language.step.interaction.Upload;
import org.skyve.metadata.sail.language.step.interaction.actions.Action;
import org.skyve.metadata.sail.language.step.interaction.actions.Cancel;
import org.skyve.metadata.sail.language.step.interaction.actions.Delete;
import org.skyve.metadata.sail.language.step.interaction.actions.Ok;
import org.skyve.metadata.sail.language.step.interaction.actions.Remove;
import org.skyve.metadata.sail.language.step.interaction.actions.Save;
import org.skyve.metadata.sail.language.step.interaction.actions.ZoomOut;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridEdit;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridLink;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridMove;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridNew;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridRemove;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridSelect;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridZoom;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridClearFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridFilter;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridFilter.FilterOperator;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridNew;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSelect;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSort;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridZoom;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionAutoComplete;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionEdit;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionNew;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionPick;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateCalendar;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMap;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMenu;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateTree;
import org.skyve.metadata.sail.language.step.interaction.session.Login;
import org.skyve.metadata.sail.language.step.interaction.session.SwitchMode;
import org.skyve.web.UserAgentType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Builds a SAIL {@link Automation} from the steps a browser recorder sent, and writes it as readable XML.
 *
 * <p>Each recorded step is a map: {@value #OP} names the op-code and the other entries are its attributes, as text.
 * The recorder also sends {@value SailRecording#STEP_ID_KEY} (a unique id), {@value #VIEW} (the top view when the
 * gesture was made, {@code module.Document} or {@code list}) and, on navigation steps, {@value #TARGET} (the
 * {@code module.Document} opened). These are not attributes of the step.
 *
 * <p>The script signs in first, as the user who recorded it, and switches to SmartClient. The password is never
 * recorded: the login takes it from the variable {@value #PASSWORD_VARIABLE}, which the test defines (or an editor
 * replaces). A gesture the recorder could not map ({@value #UNMAPPED}), and a step that is not complete, becomes an
 * {@code <!-- unmapped: ... -->} comment before the next step: a gesture is never dropped silently. Steps at the end
 * that only go back to the Control Panel to stop the recording are left out.
 *
 * <p>Threading: not thread-safe; build each script with its own instance.
 */
public final class SailRecordingBuilder {
	private static final org.slf4j.Logger LOGGER = org.skyve.util.logging.SkyveLoggerFactory.getLogger(SailRecordingBuilder.class);

	/** The key of a recorded step's op-code. */
	public static final String OP = "op";
	/** The key of the top view, {@code module.Document} or {@code list}, when the gesture was made. */
	public static final String VIEW = "view";
	/** The key of the {@code module.Document} a navigation step opened. */
	public static final String TARGET = "target";
	/** The op-code of a gesture the recorder could not map to a step; its {@code text} describes it. */
	public static final String UNMAPPED = "unmapped";
	/** The op-code of what the screen showed after a step (messages, changed values); its {@code text} says it. */
	public static final String CONTEXT = "context";
	/** The key of a description of the screen the gesture was made on: module, document, view and renderer. */
	public static final String SCREEN = "screen";
	/** The key of a data entry's field label. */
	public static final String LABEL = "label";
	/** The key of the value a data entry's field showed before the user changed it. */
	public static final String BEFORE = "before";
	/** The key of a zoomed row's bizKey. */
	public static final String ROW_BIZ_KEY = "rowBizKey";
	/** The key of the text a zoomed row's cells showed, {@value #CELL_SEPARATOR}-separated. */
	public static final String ROW_TEXTS = "rowTexts";
	/** The key of a context entry that is true when the response showed errors. */
	public static final String ERRORS = "errors";
	/**
	 * The key of an inline data grid's binding on a {@code dataEnter} into one of its cells: the step's binding is
	 * the grid's column, so it is not checked against the view's widgets.
	 */
	public static final String GRID = "grid";
	/**
	 * The key of a context entry that says the user declined a question the step before it raised (a server-side
	 * question answered No or Cancel): the step gets {@code confirm="false"}.
	 */
	public static final String DECLINED = "declined";
	/** Separates a row's cell texts in {@value #ROW_TEXTS}. */
	public static final String CELL_SEPARATOR = " | ";

	/**
	 * Checks a recorded step against the metadata, as the server knows it when recording stops.
	 */
	@FunctionalInterface
	public interface StepCheck {
		/**
		 * Returns why the step cannot be confirmed, or {@code null} if it can (or cannot be checked).
		 *
		 * @param recorded	the recorded step: its op-code, attributes and the screen it was made on
		 * @return the reason, or {@code null}
		 */
		@Nullable String unconfirmed(@Nonnull Map<String, String> recorded);
	}
	/** The variable the recorded login takes the password from. */
	public static final String PASSWORD_VARIABLE = "password";
	/** The view in which recording is started and stopped. */
	static final String CONTROL_PANEL = "admin.ControlPanel";

	/** Marks a {@link Comment} placeholder that {@link #toXml(Result)} writes as an XML comment. */
	private static final String XML_COMMENT_MARKER = "sail-recording:";
	private static final String UNMAPPED_PREFIX = "unmapped: ";
	private static final String TEXT = "text";
	private static final String MODULE = "module";
	private static final String DOCUMENT = "document";
	private static final String QUERY = "query";
	private static final String MODEL = "model";
	private static final String CONFIRM = "confirm";
	private static final String ACTION = "action";
	private static final String BINDING = "binding";
	private static final String COLUMN = "column";
	private static final String ROW_CONTAINING = "rowContaining";
	private static final String VALUE = "value";
	private static final String VARIABLE = "variable";
	/** The keys of a recorded step that are not attributes of its SAIL step. */
	private static final java.util.Set<String> CONTEXT_KEYS = java.util.Set.of(OP, VIEW, TARGET, SailRecording.STEP_ID_KEY,
																				SCREEN, LABEL, BEFORE, ROW_BIZ_KEY, ROW_TEXTS, ERRORS, GRID, DECLINED,
																				SailRecordingCaptures.SHOWN, SailRecordingCaptures.CELLS,
																				SailRecordingCaptures.TITLE, "file", "refusal");

	/**
	 * The order attributes are written in, whatever the element: the identifying attributes first, then the values,
	 * then the options. Unlisted attributes follow in alphabetic order.
	 */
	private static final List<String> ATTRIBUTE_ORDER = List.of("uxui", "userAgentType", "testStrategy",
																	"name", "customer", "user", PASSWORD_VARIABLE,
																	MODULE, DOCUMENT, QUERY, MODEL, "item", "bizId",
																	BINDING, ACTION, "path", COLUMN, "row", ROW_CONTAINING,
																	"to", "text", "search", "pick", "file",
																	VALUE, "match", "operator", "descending", VARIABLE, "pattern",
																	"createView", CONFIRM, "testSuccess");

	/**
	 * A built automation and the XML comments to write with it.
	 *
	 * @param automation	the automation
	 * @param comments	the comments to write, looked up by identity: before a {@link Step}, at the start of the
	 * 						{@link Automation}, or at the end of an {@link Interaction}'s method
	 */
	public record Result(@Nonnull Automation automation, @Nonnull IdentityHashMap<Object, List<String>> comments) {
		/**
		 * Returns the comments for an object of the automation.
		 *
		 * @param key	a step, the automation or an interaction
		 * @return the comments, in order; empty if there are none
		 */
		public @Nonnull List<String> commentsFor(@Nonnull Object key) {
			List<String> result = comments.get(key);
			return (result == null) ? List.of() : result;
		}
	}

	private final String customerName;
	private final String userName;
	private final long startedMillis;
	private final long stoppedMillis;

	private List<String> roleNames = List.of();
	private String uxui = null;
	private UserAgentType userAgentType = null;
	private StepCheck check = null;
	private final List<String> notes = new ArrayList<>(1);

	private final IdentityHashMap<Object, List<String>> comments = new IdentityHashMap<>();
	/** The screen of the last entry, for the headings. */
	private String lastScreen = null;
	/** The step of the last gesture, which a following "errors shown" context marks testSuccess="false". */
	private Step lastStep = null;
	/** Comments waiting for the next step. */
	private final List<String> pending = new ArrayList<>();

	/**
	 * Creates a builder for a recording.
	 *
	 * @param customerName	the customer to sign in to, or {@code null} when the sign in page has no customer field
	 * @param userName	the user who recorded the steps, who signs in
	 * @param startedMillis	when recording started
	 * @param stoppedMillis	when recording stopped
	 */
	public SailRecordingBuilder(@Nullable String customerName, @Nonnull String userName, long startedMillis, long stoppedMillis) {
		this.customerName = customerName;
		this.userName = userName;
		this.startedMillis = startedMillis;
		this.stoppedMillis = stoppedMillis;
	}

	/**
	 * Sets the recording user's effective roles, which the header writes ready to paste into {@code viewRoles(...)}.
	 *
	 * @param roles	the role names, {@code module.Role}; copied
	 * @return this
	 */
	public @Nonnull SailRecordingBuilder roles(@Nonnull List<String> roles) {
		roleNames = List.copyOf(roles);
		return this;
	}

	/**
	 * Sets the UX/UI the recording user got, which the header says.
	 *
	 * @param uxuiName	the UX/UI name, or {@code null} if not known
	 * @return this
	 */
	public @Nonnull SailRecordingBuilder uxui(@Nullable String uxuiName) {
		uxui = uxuiName;
		return this;
	}

	/**
	 * Sets the UX/UI and user agent type the script targets: the ones the recording user got, so that a recording made
	 * in PrimeFaces (for example {@code tablet}) replays in PrimeFaces. Without them, the script targets SmartClient
	 * ({@code desktop}).
	 *
	 * @param uxuiName	the UX/UI name, or {@code null} for desktop
	 * @param agentType	the user agent type, or {@code null} for desktop
	 * @return this
	 */
	public @Nonnull SailRecordingBuilder targeting(@Nullable String uxuiName, @Nullable UserAgentType agentType) {
		uxui = uxuiName;
		userAgentType = agentType;
		return this;
	}

	private @Nonnull String targetUxui() {
		return ((uxui == null) || (userAgentType == null)) ? UxUi.DESKTOP_NAME : uxui;
	}

	private @Nonnull UserAgentType targetUserAgentType() {
		return ((uxui == null) || (userAgentType == null)) ? UserAgentType.desktop : userAgentType;
	}

	/** The renderer the script targets, for its header. */
	private @Nonnull String renderer() {
		return UxUi.DESKTOP_NAME.equals(targetUxui()) ? "SmartClient" : "PrimeFaces (" + targetUxui() + ")";
	}

	/**
	 * Adds a note to the script's header comments, such as why its steps were not checked.
	 *
	 * @param note	the note
	 * @return this
	 */
	public @Nonnull SailRecordingBuilder note(@Nonnull String note) {
		notes.add(note);
		return this;
	}

	/**
	 * Sets the check each step must pass to be a step: one it fails becomes a comment that says why.
	 *
	 * @param stepCheck	the check, or {@code null} for none
	 * @return this
	 */
	public @Nonnull SailRecordingBuilder check(@Nullable StepCheck stepCheck) {
		check = stepCheck;
		return this;
	}

	/**
	 * Builds the automation from the recorded steps.
	 *
	 * <p>Complexity: O(n) in the number of steps.
	 *
	 * @param steps	the recorded steps, in order
	 * @return the automation (desktop UX/UI and user agent) and its comments
	 */
	public @Nonnull Result build(@Nonnull List<? extends Map<String, String>> steps) {
		comments.clear();
		pending.clear();

		Automation automation = new Automation();
		automation.setUxui(targetUxui());
		automation.setUserAgentType(targetUserAgentType());

		int end = endBeforeStoppingSteps(steps);
		comments.put(automation, header(gestures(steps, end)));

		Procedure before = new Procedure();
		Login login = new Login();
		login.setCustomer(customerName);
		login.setUser(userName);
		login.setPassword("{var:" + PASSWORD_VARIABLE + "}");
		before.getSteps().add(login);
		before.getSteps().add(new SwitchMode());
		automation.setBefore(before);

		Interaction interaction = new Interaction();
		interaction.setName("Recording");
		automation.getInteractions().add(interaction);
		List<Step> method = interaction.getSteps();
		lastScreen = null;
		lastStep = null;
		for (Map<String, String> recorded : withCaptures(steps.subList(0, end))) {
			Step step = entry(recorded, true);
			if (step != null) {
				addStep(method, step);
			}
		}
		if (! pending.isEmpty()) {
			comments.put(interaction, new ArrayList<>(pending));
			pending.clear();
		}
		return new Result(automation, new IdentityHashMap<>(comments));
	}

	/** The number of recorded gestures among the first {@code end} entries: those that are not context. */
	private static int gestures(@Nonnull List<? extends Map<String, String>> steps, int end) {
		int result = 0;
		for (int i = 0; i < end; i++) {
			if (! CONTEXT.equals(steps.get(i).get(OP))) {
				result++;
			}
		}
		return result;
	}

	/** The entries with their captures (see {@link SailRecordingCaptures}); without them if they cannot be worked out. */
	private static @Nonnull List<? extends Map<String, String>> withCaptures(@Nonnull List<? extends Map<String, String>> entries) {
		try {
			return SailRecordingCaptures.apply(entries);
		}
		catch (RuntimeException e) {
			// the script without its captures, rather than no script
			LOGGER.warn("SAIL recording could not work out the captures", e);
			return entries;
		}
	}

	/** Adds a step to the method, with the comments that go before it. */
	private void addStep(@Nonnull List<Step> method, @Nonnull Step step) {
		if (method.isEmpty() && (! isNavigation(step))) {
			pending.add(0, "The recording started in the Control Panel. Navigate to the view of the next step to replay it on its own.");
		}
		method.add(step);
		if (! pending.isEmpty()) {
			comments.put(step, new ArrayList<>(pending));
			pending.clear();
		}
	}

	/**
	 * Turns a recorded entry into its step, adding the comments that go before it to {@link #pending}: context, a
	 * heading when the screen changes, the step's own context, and an unmapped or unconfirmed gesture. A context
	 * entry that says errors were shown sets {@code testSuccess="false"} on the action before it, as a replay needs.
	 *
	 * @param recorded	the recorded entry
	 * @param checked	true to check the step ({@link #unconfirmed(Map)})
	 * @return the step, or {@code null} if the entry is a comment
	 */
	private @Nullable Step entry(@Nonnull Map<String, String> recorded, boolean checked) {
		if (CONTEXT.equals(recorded.get(OP))) {
			context(recorded);
			return null;
		}
		String stepScreen = text(recorded, SCREEN);
		if ((stepScreen != null) && (! stepScreen.equals(lastScreen))) {
			pending.add("screen: " + stepScreen);
			lastScreen = stepScreen;
		}
		Step step = safeStep(recorded);
		if ((step != null) && checked && (! confirmed(recorded))) {
			step = null;
		}
		if (step != null) {
			addOwnContext(recorded);
		}
		lastStep = step;
		return step;
	}

	/**
	 * Adds a context entry's text to the comments, and applies what it says to the step before it: errors were shown
	 * ({@code testSuccess="false"}), or a question was declined ({@code confirm="false"}).
	 */
	private void context(@Nonnull Map<String, String> recorded) {
		String text = text(recorded, TEXT);
		if (text != null) {
			pending.add(text);
		}
		if ("true".equals(recorded.get(ERRORS))) {
			if (lastStep instanceof AbstractAction action) {
				action.setTestSuccess(Boolean.FALSE);
			}
			else if (lastStep instanceof DataGridLink link) {
				link.setTestSuccess(Boolean.FALSE);
			}
			else if (lastStep instanceof Upload upload) {
				upload.setTestSuccess(Boolean.FALSE);
			}
		}
		if ("true".equals(recorded.get(DECLINED))) {
			decline(lastStep);
		}
	}

	/** The step for a gesture, or null; a step that cannot be built (a value it cannot take) is a comment, never a broken script. */
	private @Nullable Step safeStep(@Nonnull Map<String, String> recorded) {
		try {
			return step(recorded);
		}
		catch (RuntimeException e) {
			LOGGER.warn("SAIL recording could not build the step {}", recorded, e);
			pending.add(UNMAPPED_PREFIX + describe(recorded) + " - " + e);
			return null;
		}
	}

	/**
	 * Whether a step passes the checks; one that fails becomes an unconfirmed comment, and one the checks fail on is kept
	 * with a comment that it was not checked.
	 */
	private boolean confirmed(@Nonnull Map<String, String> recorded) {
		String reason;
		try {
			reason = unconfirmed(recorded);
		}
		catch (RuntimeException e) {
			LOGGER.warn("SAIL recording could not check the step {}", recorded, e);
			pending.add("not checked: " + describe(recorded) + " - " + e);
			return true;
		}
		if (reason != null) {
			pending.add("unconfirmed: " + describe(recorded) + " - " + reason);
			return false;
		}
		return true;
	}

	/**
	 * Returns what the script shows for a range of the recorded entries, for the recorder's panel: each entry's
	 * comments and its step's XML, one line each. Steps are not checked against the metadata (that happens on Stop),
	 * and the header is left out.
	 *
	 * <p>Complexity: O(from + count): the screen headings need the entries before the range.
	 *
	 * @param steps	all the recorded entries
	 * @param from	the zero-based index of the first entry
	 * @param count	the most entries to return
	 * @return for each entry in the range, its lines; a line that starts with {@code <!--} is a comment
	 */
	public @Nonnull List<List<String>> preview(@Nonnull List<? extends Map<String, String>> steps, int from, int count) {
		pending.clear();
		lastScreen = null;
		lastStep = null;
		int start = Math.max(0, Math.min(from, steps.size()));
		// in long arithmetic: start + count overflows for a large count
		int end = (int) Math.max(start, Math.min((long) start + Math.max(0, count), steps.size()));
		List<List<String>> result = new ArrayList<>(end - start);
		for (int i = 0; i < end; i++) {
			pending.clear();
			Step step = entry(steps.get(i), false);
			if (i >= start) {
				List<String> lines = new ArrayList<>(pending.size() + 1);
				for (String comment : pending) {
					lines.add("<!--" + commentText(comment) + "-->");
				}
				if (step != null) {
					lines.addAll(safeElementLines(step, steps.get(i)));
				}
				result.add(lines);
			}
		}
		pending.clear();
		return result;
	}

	/** A step's lines, or a comment if the step cannot be written. */
	private static @Nonnull List<String> safeElementLines(@Nonnull Step step, @Nonnull Map<String, String> recorded) {
		try {
			return elementLines(step);
		}
		catch (RuntimeException e) {
			LOGGER.warn("SAIL recording could not show the step {}", recorded, e);
			return List.of("<!--" + commentText(UNMAPPED_PREFIX + describe(recorded) + " - " + e) + "-->");
		}
	}

	/**
	 * Returns a step's XML as {@link #toXml(Result)} writes it, one line per line, without indentation.
	 */
	private static @Nonnull List<String> elementLines(@Nonnull Step step) {
		Automation automation = new Automation();
		automation.setUxui(UxUi.DESKTOP_NAME);
		automation.setUserAgentType(UserAgentType.desktop);
		Interaction interaction = new Interaction();
		interaction.setName("Preview");
		interaction.getSteps().add(step);
		automation.getInteractions().add(interaction);
		String xml = toXml(new Result(automation, new IdentityHashMap<>()));
		int start = xml.indexOf("<method>");
		int end = xml.indexOf("</method>");
		List<String> result = new ArrayList<>(2);
		if ((start >= 0) && (end > start)) {
			for (String line : xml.substring(start + "<method>".length(), end).split("\n")) {
				String trimmed = line.strip();
				if (! trimmed.isEmpty()) {
					result.add(trimmed);
				}
			}
		}
		return result;
	}

	/**
	 * Returns where the steps end once the steps that went back to the Control Panel to stop recording are left out:
	 * the last navigation to the Control Panel, if every step after it was made on the Control Panel.
	 *
	 * @param steps	the recorded steps
	 * @return the number of steps to build
	 */
	private static int endBeforeStoppingSteps(@Nonnull List<? extends Map<String, String>> steps) {
		for (int i = steps.size() - 1; i >= 0; i--) {
			Map<String, String> step = steps.get(i);
			if (CONTROL_PANEL.equals(step.get(TARGET))) {
				return i;
			}
			String op = step.get(OP);
			if ((! CONTROL_PANEL.equals(step.get(VIEW))) && (! UNMAPPED.equals(op)) && (! CONTEXT.equals(op))) {
				break;
			}
		}
		return steps.size();
	}

	private @Nonnull List<String> header(int stepCount) {
		DateTimeFormatter format = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm").withZone(ZoneId.systemDefault());
		List<String> result = new ArrayList<>(2);
		result.add(String.format("Recorded in %s by %s%s from %s to %s (%d recorded gestures).",
									renderer(),
									(customerName == null) ? "" : customerName + '/',
									userName,
									format.format(Instant.ofEpochMilli(startedMillis)),
									format.format(Instant.ofEpochMilli(stoppedMillis)),
									Integer.valueOf(stepCount)));
		result.add("The password is never recorded: define the variable \"" + PASSWORD_VARIABLE +
						"\" before running this script, or replace {var:" + PASSWORD_VARIABLE + "}.");
		if (! roleNames.isEmpty()) {
			StringBuilder roles = new StringBuilder(64 + roleNames.size() * 24);
			roles.append("The recorded user's roles, for the test: viewRoles(\"").append(userName).append('"');
			for (String roleName : roleNames) {
				roles.append(", \"").append(roleName).append('"');
			}
			result.add(roles.append(");").toString());
		}
		if (uxui != null) {
			result.add("The recorded user got the UX/UI " + uxui + ".");
		}
		result.addAll(notes);
		return result;
	}

	/**
	 * Returns why a step cannot be confirmed: a row it names by text that the row did not show, or what the
	 * {@link StepCheck} says.
	 *
	 * @return the reason, or {@code null} if the step is confirmed
	 */
	private @Nullable String unconfirmed(@Nonnull Map<String, String> recorded) {
		String rowContaining = recorded.get(ROW_CONTAINING);
		String rowTexts = recorded.get(ROW_TEXTS);
		if ((rowContaining != null) && (rowTexts != null) && (! rowContaining.contains("{var:")) && (! rowTexts.contains(rowContaining))) {
			return "the row did not show \"" + rowContaining + '"';
		}
		return (check == null) ? null : check.unconfirmed(recorded);
	}

	/**
	 * Adds the context a step carries for whoever edits the script: a data entry's field label and the value shown
	 * before, and a zoomed row's bizKey and values.
	 */
	private void addOwnContext(@Nonnull Map<String, String> recorded) {
		String label = text(recorded, LABEL);
		if (label != null) {
			String before = recorded.get(BEFORE);
			if (before == null) {
				pending.add('"' + label + '"');
			}
			else {
				pending.add('"' + label + "\" was " + (before.isBlank() ? "empty" : '"' + before + '"'));
			}
		}
		if ("upload".equals(recorded.get(OP))) {
			pending.add("upload: the file " + recorded.get("file") + " was chosen; put a file of that name in sail/files on the test classpath (skyve-war/src/test/resources), or change the path");
		}
		String rowTexts = text(recorded, ROW_TEXTS);
		if (rowTexts != null) {
			String bizKey = text(recorded, ROW_BIZ_KEY);
			pending.add("row: " + ((bizKey == null) ? "" : "bizKey \"" + bizKey + "\", ") + "showing " + rowTexts);
		}
	}

	private static boolean isNavigation(@Nonnull Step step) {
		return (step instanceof NavigateMenu) || (step instanceof NavigateEdit) || (step instanceof NavigateList); // trees, maps and calendars are lists
	}

	/**
	 * Makes the step for a recorded gesture, or adds a comment to {@link #pending} for a gesture that is not a step.
	 *
	 * @param recorded	the recorded gesture
	 * @return the step, or {@code null} if the gesture became a comment
	 */
	private @Nullable Step step(@Nonnull Map<String, String> recorded) {
		String op = recorded.get(OP);
		Function<Map<String, String>, Step> factory = (op == null) ? null : STEP_FACTORIES.get(op);
		Step result = (factory == null) ? null : factory.apply(recorded);
		if (result == null) {
			pending.add(UNMAPPED_PREFIX + describe(recorded));
		}
		return result;
	}

	/** Makes each op-code's step from a recorded gesture; a factory returns {@code null} for an incomplete gesture. */
	private static final Map<String, Function<Map<String, String>, Step>> STEP_FACTORIES = stepFactories();

	private static @Nonnull Map<String, Function<Map<String, String>, Step>> stepFactories() {
		Map<String, Function<Map<String, String>, Step>> result = new HashMap<>(64);
		result.put("navigateMenu", SailRecordingBuilder::navigateMenu);
		result.put("navigateEdit", SailRecordingBuilder::navigateEdit);
		result.put("navigateList", SailRecordingBuilder::navigateList);
		result.put("navigateTree", recorded -> listNavigation(new NavigateTree(), recorded));
		result.put("navigateMap", SailRecordingBuilder::navigateMap);
		result.put("navigateCalendar", recorded -> listNavigation(new NavigateCalendar(), recorded));
		result.put("dataEnter", SailRecordingBuilder::dataEnter);
		result.put("tabSelect", SailRecordingBuilder::tabSelect);
		result.put(ACTION, SailRecordingBuilder::action);
		result.put("save", recorded -> declinable(new Save(), Save::setConfirm, recorded));
		result.put("ok", recorded -> declinable(new Ok(), Ok::setConfirm, recorded));
		result.put("delete", recorded -> declinable(new Delete(), Delete::setConfirm, recorded));
		result.put("zoomOut", recorded -> declinable(new ZoomOut(), ZoomOut::setConfirm, recorded));
		result.put("remove", recorded -> declinable(new Remove(), Remove::setConfirm, recorded));
		result.put("cancel", recorded -> new Cancel());
		result.put("listGridZoom", SailRecordingBuilder::listGridZoom);
		result.put("listGridNew", SailRecordingBuilder::listGridNew);
		result.put("listGridSelect", SailRecordingBuilder::listGridSelect);
		result.put("listGridFilter", SailRecordingBuilder::listGridFilter);
		result.put("listGridClearFilter", recorded -> list(new ListGridClearFilter(), ListGridClearFilter::setModuleName,
															ListGridClearFilter::setDocumentName, ListGridClearFilter::setQueryName,
															ListGridClearFilter::setModelName, recorded));
		result.put("listGridSort", SailRecordingBuilder::listGridSort);
		result.put("dataGridNew", SailRecordingBuilder::dataGridNew);
		result.put("dataGridZoom", recorded -> gridRow(new DataGridZoom(), DataGridZoom::setBinding, DataGridZoom::setRow,
															DataGridZoom::setRowContaining, recorded));
		result.put("dataGridEdit", recorded -> gridRow(new DataGridEdit(), DataGridEdit::setBinding, DataGridEdit::setRow,
															DataGridEdit::setRowContaining, recorded));
		result.put("dataGridRemove", recorded -> gridRow(new DataGridRemove(), DataGridRemove::setBinding, DataGridRemove::setRow,
															DataGridRemove::setRowContaining, recorded));
		result.put("dataGridSelect", recorded -> gridRow(new DataGridSelect(), DataGridSelect::setBinding, DataGridSelect::setRow,
															DataGridSelect::setRowContaining, recorded));
		result.put("dataGridMove", SailRecordingBuilder::dataGridMove);
		result.put("dataGridLink", SailRecordingBuilder::dataGridLink);
		result.put("lookupDescriptionPick", recorded -> gridRow(new LookupDescriptionPick(), LookupDescriptionPick::setBinding,
																	LookupDescriptionPick::setRow, LookupDescriptionPick::setRowContaining, recorded));
		result.put("lookupDescriptionAutoComplete", SailRecordingBuilder::lookupDescriptionAutoComplete);
		result.put("lookupDescriptionNew", recorded -> bound(new LookupDescriptionNew(), LookupDescriptionNew::setBinding, recorded));
		result.put("lookupDescriptionEdit", recorded -> bound(new LookupDescriptionEdit(), LookupDescriptionEdit::setBinding, recorded));
		result.put("upload", SailRecordingBuilder::upload);
		result.put("captureValue", SailRecordingBuilder::captureValue);
		result.put("captureCell", SailRecordingBuilder::captureCell);
		result.put("captureTitle", SailRecordingBuilder::captureTitle);
		result.put("comment", SailRecordingBuilder::comment);
		result.put("testValue", SailRecordingBuilder::testValue);
		result.put("testMessage", SailRecordingBuilder::testMessage);
		result.put("testGridRows", SailRecordingBuilder::testGridRows);
		return result;
	}

	/**
	 * Describes a gesture that did not become a step: an unmapped gesture's own description, or the op-code and its
	 * attributes.
	 */
	private static @Nonnull String describe(@Nonnull Map<String, String> recorded) {
		String op = recorded.get(OP);
		if (UNMAPPED.equals(op)) {
			String text = recorded.get(TEXT);
			return (text == null) ? "a gesture" : text;
		}
		StringBuilder result = new StringBuilder(64);
		result.append((op == null) ? "a step with no op-code" : op);
		for (Map.Entry<String, String> entry : recorded.entrySet()) {
			String key = entry.getKey();
			if (! CONTEXT_KEYS.contains(key)) {
				result.append(' ').append(key).append("=\"").append(entry.getValue()).append('"');
			}
		}
		return result.toString();
	}

	private static @Nullable String text(@Nonnull Map<String, String> recorded, @Nonnull String key) {
		String value = recorded.get(key);
		if (value == null) {
			return null;
		}
		String result = value.trim();
		return result.isEmpty() ? null : result;
	}

	private static @Nullable Step navigateMenu(@Nonnull Map<String, String> recorded) {
		String module = text(recorded, MODULE);
		String item = text(recorded, "item");
		if ((module == null) || (item == null)) {
			return null;
		}
		NavigateMenu result = new NavigateMenu();
		result.setModuleName(module);
		result.setItemName(item);
		return result;
	}

	private static @Nullable Step navigateEdit(@Nonnull Map<String, String> recorded) {
		String module = text(recorded, MODULE);
		String document = text(recorded, DOCUMENT);
		if ((module == null) || (document == null)) {
			return null;
		}
		NavigateEdit result = new NavigateEdit();
		result.setModuleName(module);
		result.setDocumentName(document);
		result.setBizId(text(recorded, "bizId"));
		return result;
	}

	private static @Nullable Step navigateList(@Nonnull Map<String, String> recorded) {
		String module = text(recorded, MODULE);
		String document = text(recorded, DOCUMENT);
		String query = text(recorded, QUERY);
		if ((module == null) || ((document == null) && (query == null))) {
			return null;
		}
		NavigateList result = new NavigateList();
		result.setModuleName(module);
		result.setDocumentName(document);
		result.setQueryName(query);
		result.setModelName(text(recorded, MODEL));
		return result;
	}

	private static @Nullable Step dataEnter(@Nonnull Map<String, String> recorded) {
		String binding = text(recorded, BINDING);
		String value = recorded.get(VALUE);
		// SAIL cannot enter an empty value: a blank value counts as absent
		if ((binding == null) || (value == null) || value.isBlank()) {
			return null;
		}
		DataEnter result = new DataEnter();
		result.setBinding(binding);
		result.setValue(value);
		String match = text(recorded, "match");
		if ((match != null) && (! Match.index.name().equals(match))) {
			try {
				result.setMatch(Match.valueOf(match));
			}
			catch (@SuppressWarnings("unused") IllegalArgumentException e) {
				return null; // a match SAIL has not: an unmapped comment
			}
		}
		return result;
	}

	private static @Nullable Step tabSelect(@Nonnull Map<String, String> recorded) {
		String path = text(recorded, "path");
		if (path == null) {
			return null;
		}
		TabSelect result = new TabSelect();
		result.setTabPath(path);
		return result;
	}

	private static @Nullable Step action(@Nonnull Map<String, String> recorded) {
		String name = text(recorded, "name");
		if (name == null) {
			return null;
		}
		Action result = new Action();
		result.setActionName(name);
		return declinable(result, Action::setConfirm, recorded);
	}

	/** Sets {@code confirm="false"} on a step that takes it. */
	private static void decline(@Nullable Step step) {
		if (step instanceof Action action) {
			action.setConfirm(Boolean.FALSE);
		}
		else if (step instanceof Save save) {
			save.setConfirm(Boolean.FALSE);
		}
		else if (step instanceof Ok ok) {
			ok.setConfirm(Boolean.FALSE);
		}
		else if (step instanceof Delete delete) {
			delete.setConfirm(Boolean.FALSE);
		}
		else if (step instanceof ZoomOut zoomOut) {
			zoomOut.setConfirm(Boolean.FALSE);
		}
		else if (step instanceof Remove remove) {
			remove.setConfirm(Boolean.FALSE);
		}
		else if (step instanceof DataGridLink link) {
			link.setConfirm(Boolean.FALSE);
		}
	}

	private static boolean declined(@Nonnull Map<String, String> recorded) {
		return "false".equals(recorded.get(CONFIRM));
	}

	/**
	 * Returns the step, with {@code confirm="false"} if the user declined the confirmation it asked for.
	 *
	 * @param step	the step
	 * @param setConfirm	sets the step's confirm attribute
	 * @param recorded	the recorded gesture
	 * @return the step
	 */
	private static @Nonnull <S extends Step> S declinable(@Nonnull S step,
															@Nonnull BiConsumer<S, Boolean> setConfirm,
															@Nonnull Map<String, String> recorded) {
		if (declined(recorded)) {
			setConfirm.accept(step, Boolean.FALSE);
		}
		return step;
	}

	private static @Nullable Step listGridZoom(@Nonnull Map<String, String> recorded) {
		ListGridZoom result = list(new ListGridZoom(), ListGridZoom::setModuleName, ListGridZoom::setDocumentName,
									ListGridZoom::setQueryName, ListGridZoom::setModelName, recorded);
		return locateRow(recorded, row -> result.setRow(Integer.valueOf(row)), result::setRowContaining) ? result : null;
	}

	private static @Nullable Step listGridSelect(@Nonnull Map<String, String> recorded) {
		ListGridSelect result = list(new ListGridSelect(), ListGridSelect::setModuleName, ListGridSelect::setDocumentName,
										ListGridSelect::setQueryName, ListGridSelect::setModelName, recorded);
		return locateRow(recorded, row -> result.setRow(Integer.valueOf(row)), result::setRowContaining) ? result : null;
	}

	/**
	 * Sets a step's row from the recorded {@code row} or {@code rowContaining}, exactly one of which must be given.
	 *
	 * @return false if neither or both are given, or the row is not a number
	 */
	private static boolean locateRow(@Nonnull Map<String, String> recorded,
										@Nonnull IntConsumer setRow,
										@Nonnull Consumer<String> setRowContaining) {
		String rowContaining = recorded.get(ROW_CONTAINING);
		String row = text(recorded, "row");
		boolean hasText = (rowContaining != null) && (! rowContaining.isBlank());
		if (hasText == (row != null)) {
			return false;
		}
		if (hasText) {
			setRowContaining.accept(rowContaining);
			return true;
		}
		try {
			setRow.accept(Integer.parseInt(row));
			return true;
		}
		catch (@SuppressWarnings("unused") NumberFormatException e) {
			return false;
		}
	}

	/** Sets a list step's module, document, query and model from the recorded gesture. */
	private static @Nonnull <S extends Step> S list(@Nonnull S step,
													@Nonnull BiConsumer<S, String> setModule,
													@Nonnull BiConsumer<S, String> setDocument,
													@Nonnull BiConsumer<S, String> setQuery,
													@Nonnull BiConsumer<S, String> setModel,
													@Nonnull Map<String, String> recorded) {
		setModule.accept(step, text(recorded, MODULE));
		setDocument.accept(step, text(recorded, DOCUMENT));
		setQuery.accept(step, text(recorded, QUERY));
		setModel.accept(step, text(recorded, MODEL));
		return step;
	}

	private static @Nullable Step listGridFilter(@Nonnull Map<String, String> recorded) {
		String column = text(recorded, COLUMN);
		if (column == null) {
			return null;
		}
		ListGridFilter result = list(new ListGridFilter(), ListGridFilter::setModuleName, ListGridFilter::setDocumentName,
										ListGridFilter::setQueryName, ListGridFilter::setModelName, recorded);
		result.setColumn(column);
		String value = recorded.get(VALUE);
		if ((value != null) && (! value.isEmpty())) {
			result.setValue(value);
			String operator = text(recorded, "operator");
			if ((operator != null) && (! FilterOperator.contains.name().equals(operator))) {
				try {
					result.setOperator(FilterOperator.valueOf(operator));
				}
				catch (@SuppressWarnings("unused") IllegalArgumentException e) {
					return null;
				}
			}
		}
		return result;
	}

	private static @Nullable Step listGridSort(@Nonnull Map<String, String> recorded) {
		String column = text(recorded, COLUMN);
		if (column == null) {
			return null;
		}
		ListGridSort result = list(new ListGridSort(), ListGridSort::setModuleName, ListGridSort::setDocumentName,
									ListGridSort::setQueryName, ListGridSort::setModelName, recorded);
		result.setColumn(column);
		if ("true".equals(recorded.get("descending"))) {
			result.setDescending(Boolean.TRUE);
		}
		return result;
	}

	private static @Nullable Step listNavigation(@Nonnull NavigateList step, @Nonnull Map<String, String> recorded) {
		String module = text(recorded, MODULE);
		if ((module == null) || ((text(recorded, DOCUMENT) == null) && (text(recorded, QUERY) == null))) {
			return null;
		}
		return list(step, NavigateList::setModuleName, NavigateList::setDocumentName, NavigateList::setQueryName, NavigateList::setModelName, recorded);
	}

	private static @Nullable Step navigateMap(@Nonnull Map<String, String> recorded) {
		NavigateMap result = new NavigateMap();
		result.setGeometryBinding(text(recorded, "geometryBinding"));
		if ((result.getGeometryBinding() == null) && (text(recorded, MODEL) == null)) {
			return null; // a map needs its geometry binding, unless a model names it
		}
		return listNavigation(result, recorded);
	}

	/** A step that names only a binding. */
	private static @Nullable <S extends Step> S bound(@Nonnull S step, @Nonnull BiConsumer<S, String> setBinding, @Nonnull Map<String, String> recorded) {
		String binding = text(recorded, BINDING);
		if (binding == null) {
			return null;
		}
		setBinding.accept(step, binding);
		return step;
	}

	/** A step that names a grid (or lookup) binding and a row. */
	private static @Nullable <S extends Step> S gridRow(@Nonnull S step,
														@Nonnull BiConsumer<S, String> setBinding,
														@Nonnull ObjIntConsumer<S> setRow,
														@Nonnull BiConsumer<S, String> setRowContaining,
														@Nonnull Map<String, String> recorded) {
		if (bound(step, setBinding, recorded) == null) {
			return null;
		}
		return locateRow(recorded, row -> setRow.accept(step, row), text -> setRowContaining.accept(step, text)) ? step : null;
	}

	private static @Nullable Step dataGridNew(@Nonnull Map<String, String> recorded) {
		return bound(new DataGridNew(), DataGridNew::setBinding, recorded);
	}

	private static @Nullable Step dataGridMove(@Nonnull Map<String, String> recorded) {
		DataGridMove result = gridRow(new DataGridMove(), DataGridMove::setBinding, DataGridMove::setRow, DataGridMove::setRowContaining, recorded);
		String to = text(recorded, "to");
		if ((result == null) || (to == null)) {
			return null;
		}
		try {
			result.setTo(Integer.valueOf(to));
		}
		catch (@SuppressWarnings("unused") NumberFormatException e) {
			return null;
		}
		return result;
	}

	private static @Nullable Step dataGridLink(@Nonnull Map<String, String> recorded) {
		DataGridLink result = gridRow(new DataGridLink(), DataGridLink::setBinding, DataGridLink::setRow, DataGridLink::setRowContaining, recorded);
		String linkText = text(recorded, TEXT);
		if ((result == null) || (linkText == null)) {
			return null;
		}
		result.setText(linkText);
		return declinable(result, DataGridLink::setConfirm, recorded);
	}

	private static @Nullable Step lookupDescriptionAutoComplete(@Nonnull Map<String, String> recorded) {
		LookupDescriptionAutoComplete result = bound(new LookupDescriptionAutoComplete(), LookupDescriptionAutoComplete::setBinding, recorded);
		String search = recorded.get("search");
		if ((result == null) || (search == null) || search.isBlank()) {
			return null;
		}
		result.setSearch(search);
		String pick = recorded.get("pick");
		if ((pick != null) && (! pick.isBlank())) {
			result.setPick(pick);
		}
		return result;
	}

	private static @Nullable Step upload(@Nonnull Map<String, String> recorded) {
		String binding = text(recorded, BINDING);
		String actionName = text(recorded, ACTION);
		String file = text(recorded, "file");
		if ((file == null) || ((binding == null) == (actionName == null))) {
			return null;
		}
		Upload result = new Upload();
		result.setBinding(binding);
		result.setActionName(actionName);
		// the editor supplies the file: by convention, the kitchen sinks' upload files are in sail/files
		result.setFile("sail/files/" + file.substring(Math.max(file.lastIndexOf('/'), file.lastIndexOf('\\')) + 1));
		return result;
	}

	private static @Nullable Step captureValue(@Nonnull Map<String, String> recorded) {
		CaptureValue result = bound(new CaptureValue(), CaptureValue::setBinding, recorded);
		String variable = text(recorded, VARIABLE);
		if ((result == null) || (variable == null)) {
			return null;
		}
		result.setVariable(variable);
		return result;
	}

	private static @Nullable Step captureCell(@Nonnull Map<String, String> recorded) {
		String column = text(recorded, COLUMN);
		String variable = text(recorded, VARIABLE);
		if ((column == null) || (variable == null)) {
			return null;
		}
		CaptureCell result = list(new CaptureCell(), CaptureCell::setModuleName, CaptureCell::setDocumentName,
									CaptureCell::setQueryName, CaptureCell::setModelName, recorded);
		result.setBinding(text(recorded, BINDING));
		result.setColumn(column);
		result.setVariable(variable);
		return locateRow(recorded, row -> result.setRow(Integer.valueOf(row)), result::setRowContaining) ? result : null;
	}

	private static @Nullable Step captureTitle(@Nonnull Map<String, String> recorded) {
		String variable = text(recorded, VARIABLE);
		if (variable == null) {
			return null;
		}
		CaptureTitle result = new CaptureTitle();
		result.setVariable(variable);
		return result;
	}

	private static @Nullable Step comment(@Nonnull Map<String, String> recorded) {
		String text = text(recorded, TEXT);
		if (text == null) {
			return null;
		}
		Comment result = new Comment();
		result.setComment(text);
		return result;
	}

	private static @Nullable Step testValue(@Nonnull Map<String, String> recorded) {
		String binding = text(recorded, BINDING);
		String value = recorded.get(VALUE);
		if ((binding == null) || (value == null) || value.isBlank()) {
			return null; // SAIL cannot test an empty value
		}
		TestValue result = new TestValue();
		result.setBinding(binding);
		result.setValue(value);
		return result;
	}

	private static @Nullable Step testMessage(@Nonnull Map<String, String> recorded) {
		String contains = text(recorded, "contains");
		if (contains == null) {
			return null;
		}
		TestMessage result = new TestMessage();
		result.setContains(contains);
		return result;
	}

	private static @Nullable Step testGridRows(@Nonnull Map<String, String> recorded) {
		TestGridRows result = new TestGridRows();
		result.setBinding(text(recorded, BINDING));
		result.setModuleName(text(recorded, MODULE));
		result.setDocumentName(text(recorded, DOCUMENT));
		result.setQueryName(text(recorded, QUERY));
		result.setModelName(text(recorded, MODEL));
		result.setRowContaining(recorded.get(ROW_CONTAINING));
		try {
			String min = text(recorded, "minRows");
			String max = text(recorded, "maxRows");
			result.setMinRows((min == null) ? null : Integer.valueOf(min));
			result.setMaxRows((max == null) ? null : Integer.valueOf(max));
		}
		catch (@SuppressWarnings("unused") NumberFormatException e) {
			return null;
		}
		return result;
	}

	private static @Nonnull Step listGridNew(@Nonnull Map<String, String> recorded) {
		ListGridNew result = new ListGridNew();
		result.setModuleName(text(recorded, MODULE));
		result.setDocumentName(text(recorded, DOCUMENT));
		result.setQueryName(text(recorded, QUERY));
		result.setModelName(text(recorded, MODEL));
		return result;
	}

	/**
	 * Writes the automation as SAIL XML for people to read and edit: one step per line, indented with tabs, in the
	 * SAIL default namespace, with each element's attributes in a fixed order and the comments as XML comments.
	 * New lines, tabs and carriage returns in attribute values are written as character references, so that they
	 * read back as they were.
	 *
	 * @param result	the automation and its comments
	 * @return the XML
	 */
	public static @Nonnull String toXml(@Nonnull Result result) {
		Automation automation = result.automation();
		// Marshal a copy whose step lists hold placeholders for the comments, then turn the placeholders into comments
		Automation copy = new Automation();
		copy.setUxui(automation.getUxui());
		copy.setUserAgentType(automation.getUserAgentType());
		copy.setTestStrategy(automation.getTestStrategy());
		copy.setBefore(withPlaceholders(automation.getBefore(), result));
		for (Interaction interaction : automation.getInteractions()) {
			Interaction interactionCopy = new Interaction();
			interactionCopy.setName(interaction.getName());
			interactionCopy.setBefore(withPlaceholders(interaction.getBefore(), result));
			addWithPlaceholders(interaction.getSteps(), interactionCopy.getSteps(), result);
			for (String comment : result.commentsFor(interaction)) {
				interactionCopy.getSteps().add(placeholder(comment));
			}
			interactionCopy.setAfter(withPlaceholders(interaction.getAfter(), result));
			copy.getInteractions().add(interactionCopy);
		}
		copy.setAfter(withPlaceholders(automation.getAfter(), result));

		try {
			Document document = SecureDom4j.newSAXReader().read(new StringReader(XMLMetaData.marshalSAIL(copy)));
			Element root = document.getRootElement();
			tidy(root, Namespace.get("", XMLMetaData.SAIL_NAMESPACE));
			List<String> header = result.commentsFor(automation);
			for (int i = header.size() - 1; i >= 0; i--) {
				root.content().add(0, DocumentHelper.createComment(commentText(header.get(i))));
			}
			return write(document);
		}
		catch (Exception e) {
			throw new IllegalStateException("Could not write the SAIL recording", e);
		}
	}

	private static @Nullable Procedure withPlaceholders(@Nullable Procedure procedure, @Nonnull Result result) {
		if (procedure == null) {
			return null;
		}
		Procedure copy = new Procedure();
		addWithPlaceholders(procedure.getSteps(), copy.getSteps(), result);
		return copy;
	}

	private static void addWithPlaceholders(@Nonnull List<Step> from, @Nonnull List<Step> to, @Nonnull Result result) {
		for (Step step : from) {
			for (String comment : result.commentsFor(step)) {
				to.add(placeholder(comment));
			}
			to.add(step);
		}
	}

	private static @Nonnull Comment placeholder(@Nonnull String comment) {
		Comment result = new Comment();
		result.setComment(XML_COMMENT_MARKER + comment);
		return result;
	}

	/**
	 * Puts the element and its descendants in the SAIL default namespace, drops the schema location, orders each
	 * element's attributes and replaces the comment placeholders with XML comments.
	 */
	private static void tidy(@Nonnull Element element, @Nonnull Namespace sail) {
		element.setQName(QName.get(element.getName(), sail));
		orderAttributes(element);
		Collection<Namespace> declared = new ArrayList<>(element.declaredNamespaces());
		for (Namespace namespace : declared) {
			element.remove(namespace);
		}

		List<Node> content = element.content();
		// the marshalled layout is replaced by the writer's own
		content.removeIf(node -> (node.getNodeType() == Node.TEXT_NODE) && node.getText().isBlank());
		for (int i = 0, l = content.size(); i < l; i++) {
			if (content.get(i) instanceof Element child) {
				String comment = placeholderComment(child);
				if (comment == null) {
					tidy(child, sail);
				}
				else {
					content.set(i, DocumentHelper.createComment(commentText(comment)));
				}
			}
		}
	}

	/** Keeps an element's own (unqualified) attributes, in {@link #ATTRIBUTE_ORDER} and then alphabetic order. */
	private static void orderAttributes(@Nonnull Element element) {
		List<Attribute> attributes = new ArrayList<>(element.attributes().size());
		for (Attribute attribute : element.attributes()) {
			if (attribute.getNamespaceURI().isEmpty()) {
				attributes.add(attribute);
			}
		}
		attributes.sort(Comparator.comparingInt((Attribute attribute) -> {
			int rank = ATTRIBUTE_ORDER.indexOf(attribute.getName());
			return (rank < 0) ? Integer.MAX_VALUE : rank;
		}).thenComparing(Attribute::getName));
		element.setAttributes(attributes);
	}

	/** The comment a placeholder {@code <comment>} element stands for, or {@code null} if the element is not one. */
	private static @Nullable String placeholderComment(@Nonnull Element element) {
		String text = element.getText();
		int marker = text.indexOf(XML_COMMENT_MARKER);
		if ((! "comment".equals(element.getName())) || (marker < 0)) {
			return null;
		}
		String result = text.substring(marker + XML_COMMENT_MARKER.length());
		if (result.endsWith(XMLMetaData.CDATA_END_TAG)) {
			result = result.substring(0, result.length() - XMLMetaData.CDATA_END_TAG.length());
		}
		return result;
	}


	/**
	 * Returns the text of an XML comment, padded with spaces. A comment cannot hold {@code --} or end with {@code -},
	 * so a space is put between such hyphens.
	 */
	static @Nonnull String commentText(@Nonnull String comment) {
		String result = comment.replace("\r\n", " ").replace('\n', ' ').replace('\r', ' ');
		while (result.contains("--")) {
			result = result.replace("--", "- -");
		}
		return ' ' + result + ' ';
	}

	private static @Nonnull String write(@Nonnull Document document) throws IOException {
		OutputFormat format = new OutputFormat("\t", true, StandardCharsets.UTF_8.name());
		format.setTrimText(false);
		format.setNewLineAfterDeclaration(false);
		StringWriter result = new StringWriter(4096);
		XMLWriter writer = new XMLWriter(result, format) {
			private Namespace rootNamespace = null;
			private boolean root = true;

			@Override
			protected String escapeAttributeEntities(@SuppressWarnings("null") @Nonnull String text) {
				// dom4j leaves these as is, and an XML parser would read each as a space
				return super.escapeAttributeEntities(text).replace("\t", "&#9;").replace("\n", "&#10;").replace("\r", "&#13;");
			}

			@Override
			protected void writeNamespace(@SuppressWarnings("null") @Nonnull Namespace namespace) throws IOException {
				// the root's namespace declaration goes after its attributes, as in a hand-written script
				if (root) {
					rootNamespace = namespace;
				}
				else {
					super.writeNamespace(namespace);
				}
			}

			@Override
			protected void writeAttributes(@SuppressWarnings("null") @Nonnull Element element) throws IOException {
				super.writeAttributes(element);
				if (root) {
					root = false;
					if (rootNamespace != null) {
						super.writeNamespace(rootNamespace);
					}
				}
			}
		};
		writer.write(document);
		writer.flush();
		return result.toString();
	}
}
