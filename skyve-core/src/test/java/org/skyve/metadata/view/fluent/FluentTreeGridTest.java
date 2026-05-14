package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;

/**
 * Tests for {@link FluentTreeGrid}.
 */
@SuppressWarnings("static-method")
class FluentTreeGridTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(new FluentTreeGrid().get());
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		TreeGrid tg = new TreeGrid();
		FluentTreeGrid ftg = new FluentTreeGrid(tg);
		assertEquals(tg, ftg.get());
	}

	@Test
	void rootIdBindingSetsValue() {
		FluentTreeGrid ftg = new FluentTreeGrid().rootIdBinding("parentId");
		assertEquals("parentId", ftg.get().getRootIdBinding());
	}

	@Test
	void fromCopiesRootIdBinding() {
		TreeGrid source = new TreeGrid();
		source.setRootIdBinding("rootId");
		FluentTreeGrid ftg = new FluentTreeGrid().from(source);
		assertEquals("rootId", ftg.get().getRootIdBinding());
	}
}
