package util.sail;

import java.util.Map;

import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.execution.SailCall;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * The harness tests' and kitchen sinks' {@code <call/>} fixture: it gives back its {@code text} parameter, so a script can check that the
 * call ran with its tokens substituted. With {@code fail="assert"} it throws an {@link AssertionError} instead, and
 * with {@code fail="error"} an {@link IllegalStateException}, each naming the text, so the inline failure tests can show
 * how the harness treats each.
 */
public class EchoCall implements SailCall {
	/**
	 * Gives back the {@code text} parameter, or fails as {@code fail} says.
	 *
	 * @param parameters	{@code text}, and optionally {@code fail} ({@code assert} or {@code error})
	 * @param executor	not used
	 * @return the text, or {@code null} if there is none
	 * @throws AssertionError	with {@code fail="assert"}
	 * @throws IllegalStateException	with {@code fail="error"}
	 * @throws IllegalArgumentException	if {@code fail} is anything else
	 */
	@Override
	public @Nullable String call(@Nonnull Map<String, String> parameters, @Nonnull Executor executor) {
		String text = parameters.get("text");
		String fail = parameters.get("fail");
		if (fail == null) {
			return text;
		}
		if ("assert".equals(fail)) {
			throw new AssertionError("EchoCall was asked to fail an assertion on " + text);
		}
		if ("error".equals(fail)) {
			throw new IllegalStateException("EchoCall was asked to throw on " + text);
		}
		throw new IllegalArgumentException("EchoCall's fail parameter is assert or error, not " + fail);
	}
}
