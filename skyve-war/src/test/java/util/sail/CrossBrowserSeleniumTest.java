package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;

/**
 * Tests the launch arguments that {@link CrossBrowserSelenium} gives Chrome and Edge to stop them downloading
 * components and models into every profile, and how it merges a configuration's own {@code --disable-features} into
 * them, without a browser.
 */
class CrossBrowserSeleniumTest {
	private static final String COMPONENTS = "--disable-component-update";
	private static final String FEATURES = "--disable-features=OptimizationGuideModelDownloading,OptimizationHintsFetching," +
											"OptimizationTargetPrediction,OptimizationHints";

	/** Chrome (headed or headless) and Edge start with the component updater and the model downloads off, and nothing broader. */
	@Test
	@SuppressWarnings("static-method")
	void chromeAndEdgeStartWithoutComponentOrModelDownloads() {
		BrowserConfiguration plain = new BrowserConfiguration();
		Map<String, List<String>> arguments = Map.of("chrome", CrossBrowserSelenium.chromeArguments(plain, false),
														"chrome headless", CrossBrowserSelenium.chromeArguments(plain, true),
														"edge", CrossBrowserSelenium.edgeArguments(plain, false),
														"edge headless", CrossBrowserSelenium.edgeArguments(plain, true),
														"chrome options", args(CrossBrowserSelenium.chromeOptions(plain, true).asMap()),
														"edge options", args(CrossBrowserSelenium.edgeOptions(plain, true).asMap()));
		assertAll(arguments.entrySet().stream().map(entry -> () -> {
			String browser = entry.getKey();
			List<String> list = entry.getValue();
			assertAll(() -> assertTrue(list.contains(COMPONENTS), browser + ": " + list),
						() -> assertTrue(list.contains(FEATURES), browser + ": " + list),
						() -> assertFalse(list.contains("--disable-background-networking"), browser + ": ChromeDriver adds it"));
		}));
	}

	/** An extra --disable-features joins the built-in one, as Chrome honours only the last: one argument, each feature once. */
	@Test
	@SuppressWarnings("static-method")
	void anExtraDisableFeaturesIsMergedNotDuplicated() {
		BrowserConfiguration configuration = new BrowserConfiguration().arguments("--disable-features=Translate,OptimizationHints",
																					"--use-fake-ui-for-media-stream",
																					"--disable-features= MediaRouter ,Translate,");
		String merged = FEATURES + ",Translate,MediaRouter";
		List<String> chrome = CrossBrowserSelenium.chromeArguments(configuration, true);
		List<String> edge = CrossBrowserSelenium.edgeArguments(configuration, true);
		assertAll(() -> assertEquals(List.of(merged), disabledFeatures(chrome), "chrome: " + chrome),
					() -> assertEquals(List.of(merged), disabledFeatures(edge), "edge: " + edge),
					() -> assertEquals("--use-fake-ui-for-media-stream", chrome.get(chrome.size() - 1), "chrome: the other extra is kept, last"),
					() -> assertEquals("--use-fake-ui-for-media-stream", edge.get(edge.size() - 1), "edge: the other extra is kept, last"),
					() -> assertEquals(List.of(FEATURES), disabledFeatures(CrossBrowserSelenium.chromeArguments(new BrowserConfiguration(), false)),
										"chrome: no extra, the built-in list alone"));
	}

	private static List<String> disabledFeatures(List<String> arguments) {
		return arguments.stream().filter(argument -> argument.startsWith("--disable-features")).toList();
	}

	/** Returns the {@code args} of the options' {@code goog:chromeOptions} or {@code ms:edgeOptions}. */
	private static List<String> args(Map<String, Object> capabilities) {
		for (Object value : capabilities.values()) {
			if ((value instanceof Map<?, ?> map) && (map.get("args") instanceof List<?> list)) {
				return list.stream().map(String::valueOf).toList();
			}
		}
		return List.of();
	}
}
