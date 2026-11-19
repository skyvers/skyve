package util.sail.call;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Test;

import util.sail.browser.RequestRule.ErrorReason;

/**
 * Tests the calls' parameter checks: every message names the call, so a script's mistake says where it is. Ported from
 * basix-cloud's inspection calls.
 */
@SuppressWarnings("static-method")
class SailCallParametersTest {
	private static SailCallParameters of(Map<String, String> values) {
		return new SailCallParameters("Step", values, Set.of("a", "b", "n", "list", "reason"));
	}

	private static String message(Runnable read) {
		return assertThrows(IllegalArgumentException.class, read::run).getMessage();
	}

	@Test
	void unknownParametersAreRefusedByName() {
		assertAll(() -> assertEquals("Step doesn't take the parameter [zz]: it takes [a, b, list, n, reason]",
										message(() -> of(Map.of("zz", "1", "a", "x")))),
					() -> assertEquals("Step doesn't take the parameters [y, z]: it takes none",
										message(() -> new SailCallParameters("Step", Map.of("z", "", "y", ""), Set.of()))),
					() -> assertEquals("Step", of(Map.of()).call()));
	}

	@Test
	void requiredAndOptional() {
		SailCallParameters p = of(Map.of("a", "x", "b", "  "));
		assertAll(() -> assertEquals("x", p.required("a")),
					() -> assertNull(p.optional("b"), "blank is left out"),
					() -> assertNull(p.optional("n")),
					() -> assertEquals("Step needs the parameter b", message(() -> p.required("b"))),
					() -> assertEquals("Step needs the parameter n", message(() -> p.required("n"))));
	}

	@Test
	void booleans() {
		assertAll(() -> assertTrue(of(Map.of("a", " TRUE ")).bool("a", false)),
					() -> assertFalse(of(Map.of("a", "false")).bool("a", true)),
					() -> assertTrue(of(Map.of()).bool("a", true), "the fallback"),
					() -> assertEquals("Step's a must be true or false, not yes", message(() -> of(Map.of("a", "yes")).bool("a", false))));
	}

	@Test
	void integers() {
		assertAll(() -> assertEquals(5, of(Map.of("n", " 5 ")).integer("n", 1, 0)),
					() -> assertEquals(7, of(Map.of()).integer("n", 7, 0), "the fallback"),
					() -> assertEquals(-1, of(Map.of()).integer("n", -1, 0), "a fallback below the minimum"),
					() -> assertEquals(503, of(Map.of("n", "503")).requiredInteger("n", 100)),
					() -> assertEquals("Step's n must be a whole number, not 1.5", message(() -> of(Map.of("n", "1.5")).integer("n", 0, 0))),
					() -> assertEquals("Step's n must be 1 or more, not 0", message(() -> of(Map.of("n", "0")).integer("n", 1, 1))),
					() -> assertEquals("Step's n must be 100 or more, not 99", message(() -> of(Map.of("n", "99")).requiredInteger("n", 100))),
					() -> assertEquals("Step needs the parameter n", message(() -> of(Map.of()).requiredInteger("n", 100))));
	}

	@Test
	void lists() {
		assertAll(() -> assertEquals(List.of("a.jpg", "b c.jpg"), of(Map.of("list", " a.jpg, ,b c.jpg,")).list("list")),
					() -> assertEquals(List.of(), of(Map.of()).list("list")));
	}

	@Test
	void choices() {
		assertAll(() -> assertEquals(ErrorReason.TIMED_OUT, of(Map.of("reason", "TimedOut")).choice("reason", ErrorReason.class, ErrorReason.FAILED)),
					() -> assertEquals(ErrorReason.TIMED_OUT, of(Map.of("reason", "timed_out")).choice("reason", ErrorReason.class, ErrorReason.FAILED)),
					() -> assertEquals(ErrorReason.FAILED, of(Map.of()).choice("reason", ErrorReason.class, ErrorReason.FAILED), "the fallback"),
					() -> assertTrue(message(() -> of(Map.of("reason", "Busy")).choice("reason", ErrorReason.class, ErrorReason.FAILED))
										.startsWith("Step's reason must be one of [FAILED, ABORTED, TIMED_OUT"),
										"lists the choices"));
	}
}
