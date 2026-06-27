package org.skyve.impl.metadata.module.query;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.metadata.module.Module;

@ExtendWith(MockitoExtension.class)
class MetaDataQueryReferenceImplTest {

	@Mock
	private Module owningModule;

	@Test
	@SuppressWarnings("static-method")
	void constructorStoresName() {
		MetaDataQueryReferenceImpl ref = new MetaDataQueryReferenceImpl("myQuery", "admin", "ContactQuery");
		assertThat(ref.getName(), is("myQuery"));
	}

	@Test
	@SuppressWarnings("static-method")
	void constructorStoresModuleRef() {
		MetaDataQueryReferenceImpl ref = new MetaDataQueryReferenceImpl("myQuery", "admin", "ContactQuery");
		assertThat(ref.getModuleRef(), is("admin"));
	}

	@Test
	@SuppressWarnings("static-method")
	void constructorStoresRef() {
		MetaDataQueryReferenceImpl ref = new MetaDataQueryReferenceImpl("myQuery", "admin", "ContactQuery");
		assertThat(ref.getRef(), is("ContactQuery"));
	}

	@Test
	@SuppressWarnings("static-method")
	void nullNameFallsBackToRef() {
		MetaDataQueryReferenceImpl ref = new MetaDataQueryReferenceImpl(null, "admin", "ContactQuery");
		assertThat(ref.getName(), is("ContactQuery"));
	}

	@Test
	void setOwningModuleRoundtrip() {
		MetaDataQueryReferenceImpl ref = new MetaDataQueryReferenceImpl("q", "admin", "ContactQuery");
		ref.setOwningModule(owningModule);
		assertThat(ref.getOwningModule(), is(owningModule));
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultOwningModuleIsNull() {
		MetaDataQueryReferenceImpl ref = new MetaDataQueryReferenceImpl("q", "admin", "ContactQuery");
		assertNull(ref.getOwningModule());
	}

	@Test
	@SuppressWarnings("static-method")
	void toStringContainsFields() {
		MetaDataQueryReferenceImpl ref = new MetaDataQueryReferenceImpl("myQuery", "admin", "ContactQuery");
		String str = ref.toString();
		assertThat(str, containsString("admin"));
		assertThat(str, containsString("ContactQuery"));
	}
}
