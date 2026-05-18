package org.skyve.impl.generate.client;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class RenderedComponentTest {

	private RenderedComponent component;

	@BeforeEach
	void setUp() {
		component = new RenderedComponent();
	}

	@Test
	void defaultOutputIsEmpty() {
		assertEquals(0, component.getOutput().length());
	}

	@Test
	void defaultParentIsNull() {
		assertNull(component.getParent());
	}

	@Test
	void isLeafWhenNoChildren() {
		assertTrue(component.isLeaf());
	}

	@Test
	void addChildSetsParent() {
		RenderedComponent child = new RenderedComponent();
		component.addChild(child);
		assertThat(child.getParent(), sameInstance(component));
	}

	@Test
	void addChildMakesParentNonLeaf() {
		component.addChild(new RenderedComponent());
		assertFalse(component.isLeaf());
	}

	@Test
	void addChildAtIndexSetsParent() {
		RenderedComponent child1 = new RenderedComponent();
		RenderedComponent child2 = new RenderedComponent();
		component.addChild(child1);
		component.addChild(0, child2);
		assertThat(child2.getParent(), sameInstance(component));
		assertThat(component.getChild(0), sameInstance(child2));
		assertThat(component.getChild(1), sameInstance(child1));
	}

	@Test
	void removeChildClearsParent() {
		RenderedComponent child = new RenderedComponent();
		component.addChild(child);
		component.removeChild(child);
		assertThat(child.getParent(), is(nullValue()));
		assertTrue(component.isLeaf());
	}

	@Test
	void setAfterReturnsThis() {
		RenderedComponent result = component.setAfter("after");
		assertThat(result, sameInstance(component));
	}

	@Test
	void setIndentReturnsThis() {
		RenderedComponent result = component.setIndent("  ");
		assertThat(result, sameInstance(component));
	}

	@Test
	void toStringWithNoChildrenAndNoIndent() {
		component.getOutput().append("hello");
		assertThat(component.toString(), is("hello"));
	}

	@Test
	void toStringWithIndentAddsNewline() {
		component.getOutput().append("root");
		component.setIndent("");
		// indent="" means newline is added after root output
		String result = component.toString();
		assertTrue(result.contains("root"));
		assertTrue(result.contains("\n"));
	}

	@Test
	void toStringWithAfterAndNoIndent() {
		component.getOutput().append("before");
		component.setAfter("after");
		assertThat(component.toString(), is("beforeafter"));
	}

	@Test
	void toStringWithChildrenAppendsChildOutput() {
		component.getOutput().append("parent");
		RenderedComponent child = new RenderedComponent();
		child.getOutput().append("child");
		component.addChild(child);
		assertThat(component.toString(), is("parentchild"));
	}

	@Test
	@SuppressWarnings("static-method")
	void customIndentationConstructor() {
		RenderedComponent c = new RenderedComponent("    ");
		c.getOutput().append("content");
		c.setIndent("");
		RenderedComponent child = new RenderedComponent("    ");
		child.getOutput().append("inner");
		c.addChild(child);
		String result = c.toString();
		assertTrue(result.contains("content"));
		assertTrue(result.contains("inner"));
	}
}
