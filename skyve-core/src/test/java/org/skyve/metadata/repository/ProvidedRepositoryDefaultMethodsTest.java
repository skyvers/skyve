package org.skyve.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

@SuppressWarnings("static-method")
class ProvidedRepositoryDefaultMethodsTest {
	@Test
	void findNearestPersistentSingleOrJoinedSuperDocumentReturnsNullWithoutInheritance() {
		ProvidedRepository repository = repository((method, args) -> unsupported(method));
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		assertNull(repository.findNearestPersistentSingleOrJoinedSuperDocument(null, module, document));
	}

	@Test
	void findNearestPersistentSingleOrJoinedSuperDocumentThrowsWhenInheritedDocumentMissing() {
		ProvidedRepository repository = repository((method, args) -> unsupported(method));
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Extends inherits = new Extends();
		inherits.setDocumentName("BaseDoc");

		when(document.getExtends()).thenReturn(inherits);
		when(document.getName()).thenReturn("ChildDoc");
		when(module.getName()).thenReturn("admin");
		when(module.getDocument(null, "BaseDoc")).thenReturn(null);

		assertThrows(MetaDataException.class,
				() -> repository.findNearestPersistentSingleOrJoinedSuperDocument(null, module, document));
	}

	@Test
	void findNearestPersistentSingleOrJoinedSuperDocumentReturnsNullForMappedBaseWithoutFurtherInheritance() {
		ProvidedRepository repository = repository((method, args) -> unsupported(method));
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Document mappedBase = mock(Document.class);
		Extends inherits = new Extends();
		inherits.setDocumentName("MappedBase");
		Persistent persistent = new Persistent();
		persistent.setStrategy(ExtensionStrategy.mapped);

		when(document.getExtends()).thenReturn(inherits);
		when(module.getDocument(null, "MappedBase")).thenReturn(mappedBase);
		when(mappedBase.getPersistent()).thenReturn(persistent);

		assertNull(repository.findNearestPersistentSingleOrJoinedSuperDocument(null, module, document));
	}

	@Test
	void findNearestPersistentSingleOrJoinedSuperDocumentRecursesPastMappedBaseToJoinedBase() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Module baseModule = mock(Module.class);
		Document document = mock(Document.class);
		Document mappedBase = mock(Document.class);
		Document joinedBase = mock(Document.class);
		ProvidedRepository repository = repository((method, args) -> {
			if ("getModule".equals(method.getName()) && args.length == 2 && customer.equals(args[0])
					&& "baseModule".equals(args[1])) {
				return baseModule;
			}
			return unsupported(method);
		});

		Extends inherits = new Extends();
		inherits.setDocumentName("MappedBase");
		Extends baseInherits = new Extends();
		baseInherits.setDocumentName("JoinedBase");

		Persistent mappedPersistent = new Persistent();
		mappedPersistent.setStrategy(ExtensionStrategy.mapped);
		Persistent joinedPersistent = new Persistent();
		joinedPersistent.setName("JOINED_TABLE");
		joinedPersistent.setStrategy(ExtensionStrategy.joined);

		when(document.getExtends()).thenReturn(inherits);
		when(module.getDocument(customer, "MappedBase")).thenReturn(mappedBase);
		when(mappedBase.getPersistent()).thenReturn(mappedPersistent);
		when(mappedBase.getExtends()).thenReturn(baseInherits);
		when(mappedBase.getOwningModuleName()).thenReturn("baseModule");
		when(baseModule.getDocument(customer, "JoinedBase")).thenReturn(joinedBase);
		when(joinedBase.getPersistent()).thenReturn(joinedPersistent);

		assertSame(joinedBase,
				repository.findNearestPersistentSingleOrJoinedSuperDocument(customer, module, document));
	}

	@Test
	void findNearestPersistentSingleOrJoinedSuperDocumentReturnsNullWhenResolvedBaseHasNoPersistentName() {
		ProvidedRepository repository = repository((method, args) -> unsupported(method));
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Document baseDocument = mock(Document.class);
		Extends inherits = new Extends();
		inherits.setDocumentName("BaseDoc");
		Persistent persistent = new Persistent();
		persistent.setStrategy(ExtensionStrategy.single);

		when(document.getExtends()).thenReturn(inherits);
		when(module.getDocument(null, "BaseDoc")).thenReturn(baseDocument);
		when(baseDocument.getPersistent()).thenReturn(persistent);

		assertNull(repository.findNearestPersistentSingleOrJoinedSuperDocument(null, module, document));
	}

	@Test
	void findNearestPersistentSingleOrJoinedSuperDocumentReturnsNullWhenResolvedBaseHasNoPersistent() {
		ProvidedRepository repository = repository((method, args) -> unsupported(method));
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Document baseDocument = mock(Document.class);
		Extends inherits = new Extends();
		inherits.setDocumentName("BaseDoc");

		when(document.getExtends()).thenReturn(inherits);
		when(module.getDocument(null, "BaseDoc")).thenReturn(baseDocument);
		when(baseDocument.getPersistent()).thenReturn(null);

		assertNull(repository.findNearestPersistentSingleOrJoinedSuperDocument(null, module, document));
	}

	@Test
	void findNearestPersistentSingleOrJoinedSuperDocumentReturnsNullWhenResolvedBaseUsesMappedStrategy() {
		ProvidedRepository repository = repository((method, args) -> unsupported(method));
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Document baseDocument = mock(Document.class);
		Extends inherits = new Extends();
		Persistent persistent = new Persistent();
		inherits.setDocumentName("BaseDoc");
		persistent.setName("BASE_TABLE");
		persistent.setStrategy(ExtensionStrategy.mapped);

		when(document.getExtends()).thenReturn(inherits);
		when(module.getDocument(null, "BaseDoc")).thenReturn(baseDocument);
		when(baseDocument.getPersistent()).thenReturn(persistent);

		assertNull(repository.findNearestPersistentSingleOrJoinedSuperDocument(null, module, document));
	}

	private static ProvidedRepository repository(RepositoryStub stub) {
		InvocationHandler handler = (proxy, method, args) -> {
			if (method.isDefault()) {
				return invokeDefault(proxy, method, args);
			}
			if (method.getDeclaringClass() == Object.class) {
				return switch (method.getName()) {
				case "toString" -> "ProvidedRepositoryDefaultMethodsTestProxy";
				case "hashCode" -> Integer.valueOf(System.identityHashCode(proxy));
				case "equals" -> Boolean.valueOf(proxy == args[0]);
				default -> unsupported(method);
				};
			}
			return stub.apply(method, (args == null) ? new Object[0] : args);
		};
		return (ProvidedRepository) Proxy.newProxyInstance(
				ProvidedRepository.class.getClassLoader(),
				new Class<?>[] { ProvidedRepository.class },
				handler);
	}

	private static Object invokeDefault(Object proxy, Method method, Object[] args) throws Throwable {
		MethodHandles.Lookup lookup = MethodHandles.privateLookupIn(method.getDeclaringClass(), MethodHandles.lookup());
		return lookup.findSpecial(
				method.getDeclaringClass(),
				method.getName(),
				MethodType.methodType(method.getReturnType(), method.getParameterTypes()),
				method.getDeclaringClass())
				.bindTo(proxy)
				.invokeWithArguments((args == null) ? new Object[0] : args);
	}

	private static Object unsupported(Method method) {
		throw new UnsupportedOperationException(method.toString());
	}

	@FunctionalInterface
	private interface RepositoryStub {
		Object apply(Method method, Object[] args) throws Throwable;
	}
}