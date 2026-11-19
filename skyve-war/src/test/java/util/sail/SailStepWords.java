package util.sail;

import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.Call;
import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.Execute;
import org.skyve.metadata.sail.language.step.ExecuteAsync;
import org.skyve.metadata.sail.language.step.Pause;
import org.skyve.metadata.sail.language.step.TestScript;
import org.skyve.metadata.sail.language.step.WaitFor;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateLink;
import org.skyve.metadata.sail.language.step.interaction.session.Login;
import org.skyve.metadata.sail.language.step.interaction.session.Logout;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Puts a SAIL step in plain words for the media pages: what it does, without the SAIL markup. The words come from the
 * step itself (not its XML), when it starts, and hide any password a call is given.
 */
final class SailStepWords {
	/** The longest a value or script is shown in a step's summary. */
	private static final int SUMMARY_VALUE_MAX = 140;

	/**
	 * Returns a step in plain words, for the step lists: what it does, without the SAIL markup. A comment is its text,
	 * a test script its message, a call its class's name in words with its parameters, a script or a wait its code
	 * (shortened), a login who signs in, and any other step its op-code in words with its simple properties.
	 *
	 * @param step	the step
	 * @return the summary
	 */
	static @Nonnull String summary(@Nonnull Step step) {
		if (step instanceof Comment comment) {
			return shorten(text(comment.getComment()), 400);
		}
		if (step instanceof TestScript test) {
			return "Check: " + ((test.getMessage() == null) ? script(text(test.getScript())) : test.getMessage());
		}
		if (step instanceof WaitFor wait) {
			return "Wait for: " + script(text(wait.getScript()));
		}
		if (step instanceof Execute execute) {
			return scriptRun(execute.getScript(), execute.getVariable());
		}
		if (step instanceof ExecuteAsync execute) {
			return scriptRun(execute.getScript(), execute.getVariable());
		}
		if (step instanceof Call call) {
			Map<String, String> parameters = new LinkedHashMap<>();
			// a call is validated when it runs, so a class or parameter may be missing here
			call.getParameters().forEach(parameter -> parameters.put(text(parameter.getName()), SailMedia.hidden(text(parameter.getName()), text(parameter.getValue()))));
			String className = text(call.getClassName());
			return words(simpleName(className.isEmpty() ? "call" : className)) + pairs(parameters) + into(call.getVariable());
		}
		if (step instanceof Login login) {
			return "Sign in as " + text(login.getUser()) + (Boolean.TRUE.equals(login.getRemember()) ? ", remembered" : "");
		}
		if (step instanceof Logout) {
			return "Sign out";
		}
		if (step instanceof NavigateLink link) {
			return "Go to " + ((link.getHref() == null) ? text(link.getText()) : link.getHref());
		}
		if (step instanceof Pause pause) {
			return "Pause " + pause.getMillis() + " ms";
		}
		Map<String, String> shown = new TreeMap<>(InterpretedWebDriverExecutor.properties(step));
		shown.remove("timeoutMillis");
		return words(InterpretedWebDriverExecutor.opCode(step)) + pairs(shown);
	}

	private static @Nonnull String text(@Nullable String text) {
		return (text == null) ? "" : text;
	}

	private static @Nonnull String scriptRun(@Nullable String script, @Nullable String variable) {
		return "Run: " + script(text(script)) + into(variable);
	}

	private static @Nonnull String into(@Nullable String variable) {
		return (variable == null) ? "" : " (into " + variable + ")";
	}

	/** A script as one shortened line, without a leading {@code return} or a trailing semicolon. */
	static @Nonnull String script(@Nonnull String code) {
		String result = code.replaceAll("\\s+", " ").trim();
		if (result.startsWith("return ")) {
			result = result.substring(7);
		}
		if (result.endsWith(";")) {
			result = result.substring(0, result.length() - 1).trim();
		}
		return shorten(result, SUMMARY_VALUE_MAX);
	}

	/** Shortens text to at most {@code max} characters, ending in an ellipsis when it was cut. */
	static @Nonnull String shorten(@Nonnull String text, int max) {
		String result = text.replaceAll("\\s+", " ").trim();
		return (result.length() <= max) ? result : result.substring(0, max - 1).trim() + '…';
	}

	/** Names as {@code (name: value, ...)}, each value shortened; empty when there are none. */
	private static @Nonnull String pairs(@Nonnull Map<String, String> values) {
		if (values.isEmpty()) {
			return "";
		}
		StringBuilder result = new StringBuilder(" (");
		values.forEach((name, value) -> {
			if (result.length() > 2) {
				result.append(", ");
			}
			result.append(name).append(": ").append(shorten(value, SUMMARY_VALUE_MAX));
		});
		return result.append(')').toString();
	}

	/** A class's name without its package (and outer classes). */
	private static @Nonnull String simpleName(@Nonnull String className) {
		return className.substring(Math.max(className.lastIndexOf('.'), className.lastIndexOf('$')) + 1);
	}

	/** A camel-case name in words, e.g. {@code ReplaceFrontElevation} as "Replace front elevation"; acronyms stay. */
	static @Nonnull String words(@Nonnull String name) {
		// a space where a word starts: after a lower case letter or digit, or at the last capital of an acronym
		String[] parts = name.replaceAll("(?<=[a-z0-9])(?=[A-Z])", " ").replaceAll("(?<=[A-Z])(?=[A-Z][a-z])", " ").trim().split(" ");
		StringBuilder result = new StringBuilder(name.length() + 8);
		for (int i = 0; i < parts.length; i++) {
			String part = parts[i];
			if (part.isEmpty()) {
				continue;
			}
			if (i == 0) {
				result.append(Character.toUpperCase(part.charAt(0))).append(part.substring(1));
			}
			else {
				boolean acronym = (part.length() > 1) && part.equals(part.toUpperCase(Locale.ROOT));
				result.append(' ').append(acronym ? part : part.toLowerCase(Locale.ROOT));
			}
		}
		return result.toString();
	}

	private SailStepWords() {
		// static methods only
	}
}
