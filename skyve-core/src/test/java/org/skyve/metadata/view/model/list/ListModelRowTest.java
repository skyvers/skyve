package org.skyve.metadata.view.model.list;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;

public class ListModelRowTest {

	private static Map<String, Object> emptyMap() {
		return new HashMap<>();
	}

	@Test
	@SuppressWarnings("static-method")
	public void constructorSetsModuleAndDocument() {
		ListModelRow row = new ListModelRow("myModule", "MyDoc", emptyMap());
		assertThat(row.getBizModule(), is("myModule"));
		assertThat(row.getBizDocument(), is("MyDoc"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setBizLockWithoutPropertyUsesField() {
		ListModelRow row = new ListModelRow("mod", "doc", emptyMap());
		assertNull(row.getBizLock());
		row.setBizLock("lockval");
		assertThat(row.getBizLock(), is("lockval"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setBizLockWithPropertyUsesMap() {
		Map<String, Object> props = new HashMap<>();
		props.put(PersistentBean.LOCK_NAME, null);
		ListModelRow row = new ListModelRow("mod", "doc", props);
		row.setBizLock("dynLock");
		assertThat(row.getBizLock(), is("dynLock"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setBizTaggedWithoutPropertyUsesField() {
		ListModelRow row = new ListModelRow("mod", "doc", emptyMap());
		assertNull(row.getBizTagged());
		row.setBizTagged("true");
		assertThat(row.getBizTagged(), is("true"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setBizFlagCommentWithoutPropertyUsesField() {
		ListModelRow row = new ListModelRow("mod", "doc", emptyMap());
		assertNull(row.getBizFlagComment());
		row.setBizFlagComment("some comment");
		assertThat(row.getBizFlagComment(), is("some comment"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setBizKeyWithoutPropertyUsesField() {
		ListModelRow row = new ListModelRow("mod", "doc", emptyMap());
		assertNull(row.getBizKey());
		row.setBizKey("myKey");
		assertThat(row.getBizKey(), is("myKey"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getBizKeyFromMapWhenPropertyPresent() {
		Map<String, Object> props = new HashMap<>();
		props.put(Bean.BIZ_KEY, "mapKey");
		ListModelRow row = new ListModelRow("mod", "doc", props);
		assertThat(row.getBizKey(), is("mapKey"));
	}
}
