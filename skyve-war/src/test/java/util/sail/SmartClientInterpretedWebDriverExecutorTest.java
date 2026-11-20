package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.sc.Locator;
import org.skyve.impl.sail.execution.sc.Locator.Kind;
import org.skyve.impl.sail.execution.sc.SmartClientAutomationContext;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridRemove;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridSelect;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridZoom;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSelect;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Tests that the SmartClient executor's data grid and list grid gestures check for error messages straight after the
 * gesture, as the PrimeFaces executor does, so that a gesture the server refused (a zoom into a row the conversation no
 * longer holds, say) fails at that step instead of being dismissed by the next one.
 */
class SmartClientInterpretedWebDriverExecutorTest {
	private static final String PROBLEM = "warn message: An error occurred while processing your request.";

	private SmartClientSelenide selenide;
	private TestExecutor executor;

	/**
	 * Exposes {@link #push(Object)}, so that a test can give the executor a view without generating its locators from
	 * metadata.
	 */
	private static final class TestExecutor extends SmartClientInterpretedWebDriverExecutor {
		TestExecutor(@Nonnull SmartClientSelenide selenide) {
			super(selenide);
		}

		void show(@Nonnull SmartClientAutomationContext context) {
			push(context);
		}
	}

	/**
	 * Gives the executor a view whose locators are the ones named.
	 *
	 * @param identifiers	the locator identifiers, such as {@code lines.zoom}
	 */
	private void view(@Nonnull String... identifiers) {
		Map<String, List<Locator>> locators = new TreeMap<>();
		for (String identifier : identifiers) {
			locators.put(identifier, List.of(Locator.widget(null, Kind.dataGrid, identifier, null, null, null)));
		}
		SmartClientAutomationContext context = new SmartClientAutomationContext() {
			@Override
			public @Nullable List<Locator> getLocators(@Nonnull String identifier) {
				return locators.get(identifier);
			}
		};
		context.setModuleName("m");
		context.setDocumentName("D");
		executor.show(context);
	}

	@BeforeEach
	void setUp() {
		selenide = mock(SmartClientSelenide.class);
		executor = new TestExecutor(selenide);
		// settings from no sources: never this JVM's system properties, environment or properties file
		executor.useSettings(SailSettings.resolve(SailSettings.Environment.none(), List.of()));
		when(Boolean.valueOf(selenide.dataGridSelect(any()))).thenReturn(Boolean.TRUE);
		when(Boolean.valueOf(selenide.dataGridButton(any()))).thenReturn(Boolean.TRUE);
		when(Boolean.valueOf(selenide.listGridSelect(any()))).thenReturn(Boolean.TRUE);
		when(selenide.describeMessages()).thenReturn(PROBLEM);
	}

	/** The zoom's own check fails, after the row select's check passed: the failure names the zoom and the problem. */
	@Test
	void dataGridZoomFailsAtTheZoomWhenAnErrorShows() {
		view("lines.select", "lines.zoom");
		when(Boolean.valueOf(selenide.verifySuccess())).thenReturn(Boolean.TRUE, Boolean.FALSE);
		DataGridZoom zoom = new DataGridZoom();
		zoom.setBinding("lines");
		zoom.setRow(Integer.valueOf(2));

		AssertionError e = assertThrows(AssertionError.class, () -> executor.executeDataGridZoom(zoom));

		assertAll(() -> assertTrue(e.getMessage().contains("expected no error messages after <DataGridZoom /> but found: " + PROBLEM),
									e.getMessage()),
					() -> verify(selenide).dataGridButton(any()),
					() -> verify(selenide, times(2)).verifySuccess());
	}

	/** A select and a remove that show no error pass, each having checked once (the remove's select checks too). */
	@Test
	void dataGridSelectAndRemoveCheckForErrors() {
		view("lines.select", "lines.remove");
		when(Boolean.valueOf(selenide.verifySuccess())).thenReturn(Boolean.TRUE);
		DataGridSelect select = new DataGridSelect();
		select.setBinding("lines");
		select.setRow(Integer.valueOf(0));
		DataGridRemove remove = new DataGridRemove();
		remove.setBinding("lines");
		remove.setRow(Integer.valueOf(1));

		executor.executeDataGridSelect(select);
		executor.executeDataGridRemove(remove);

		verify(selenide, times(3)).verifySuccess();
	}

	/** A select whose server call failed fails at the select, before anything else is clicked. */
	@Test
	void dataGridSelectFailsWhenAnErrorShows() {
		view("lines.select", "lines.zoom");
		when(Boolean.valueOf(selenide.verifySuccess())).thenReturn(Boolean.FALSE);
		DataGridZoom zoom = new DataGridZoom();
		zoom.setBinding("lines");
		zoom.setRow(Integer.valueOf(0));

		AssertionError e = assertThrows(AssertionError.class, () -> executor.executeDataGridZoom(zoom));

		assertAll(() -> assertTrue(e.getMessage().contains("expected no error messages after <DataGridSelect />"), e.getMessage()),
					() -> verify(selenide, never()).dataGridButton(any()));
	}

	/** A list grid select whose selected event failed fails at the select. */
	@Test
	void listGridSelectFailsWhenAnErrorShows() {
		view("m.q.select");
		when(Boolean.valueOf(selenide.verifySuccess())).thenReturn(Boolean.FALSE);
		ListGridSelect select = new ListGridSelect();
		select.setModuleName("m");
		select.setQueryName("q");
		select.setRow(Integer.valueOf(0));

		AssertionError e = assertThrows(AssertionError.class, () -> executor.executeListGridSelect(select));

		assertTrue(e.getMessage().contains("expected no error messages after <ListGridSelect /> but found: " + PROBLEM),
					e.getMessage());
	}

	/** Under the None strategy nothing is checked, as for every other check. */
	@Test
	void noneStrategyDoesNotCheck() {
		executor.useSettings(SailSettings.resolve(SailSettings.Environment.none(),
													List.of(SailSettings.code().testStrategy(TestStrategy.None).layer())));
		view("lines.select");
		DataGridSelect select = new DataGridSelect();
		select.setBinding("lines");
		select.setRow(Integer.valueOf(0));

		executor.executeDataGridSelect(select);

		verify(selenide, never()).verifySuccess();
	}
}
