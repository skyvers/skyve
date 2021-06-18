package org.skyve.impl.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;

import java.util.Map;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class UtilImplTest {
	
	private ClassLoader classLoader;
	private Map<String, Object> api;
	private Map<String, String> twilio;
	
	@Before
	public void setup() throws Exception {
		classLoader = getClass().getClassLoader();
	}
	
	@Test
	@SuppressWarnings("unchecked")
	public void testReadJSONConfigNoComments() throws Exception {
		// call the method under test
		Map<String, Object> json = UtilImpl.readJSONConfig(classLoader.getResourceAsStream("json/withoutComments.json"));
		
		// verify the result
		api = (Map<String, Object>) json.get("api");
		twilio = (Map<String, String>) api.get("twilio");
		
		assertThat(api, is(notNullValue()));
		assertThat(api.get("twilio"), is(notNullValue()));
		assertThat(twilio.get("defaultSendNumber"), is(notNullValue()));
	}
	
	@Test
	@SuppressWarnings("unchecked")
	public void testReadJSONConfigWithComments() throws Exception {
		// call the method under test
		Map<String, Object> json = UtilImpl.readJSONConfig(classLoader.getResourceAsStream("json/withComments.json"));
		
		// verify the result
		api = (Map<String, Object>) json.get("api");
		twilio = (Map<String, String>) api.get("twilio");
		
		assertThat(api, is(notNullValue()));
		assertThat(api.get("twilio"), is(notNullValue()));
		assertThat(twilio.get("defaultSendNumber"), is(notNullValue()));
	}
	
	@Test
	@SuppressWarnings("unchecked")
	public void testReadJSONConfigBlockComments() throws Exception {
		// call the method under test
		Map<String, Object> json = UtilImpl.readJSONConfig(classLoader.getResourceAsStream("json/blockComments.json"));
		
		// verify the result
		api = (Map<String, Object>) json.get("api");
		twilio = (Map<String, String>) api.get("twilio");
		
		assertThat(api, is(notNullValue()));
		assertThat(api.get("twilio"), is(notNullValue()));
		assertThat(twilio.get("defaultSendNumber"), is(notNullValue()));
	}
	
	@Test
	@SuppressWarnings("unchecked")
	public void testReadJSONConfigFull() throws Exception {
		// call the method under test
		Map<String, Object> json = UtilImpl.readJSONConfig(classLoader.getResourceAsStream("json/skyve.json"));
		
		// verify the result
		Map<String, Object> environment = (Map<String, Object>) json.get("environment");
		
		assertThat(environment, is(notNullValue()));
		assertThat(environment.get("customer"), is(notNullValue()));
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testUnidecode() {
		Assert.assertEquals("descricao", UtilImpl.unidecode("descrição"));
		Assert.assertEquals("tache", UtilImpl.unidecode("tâche"));
		Assert.assertEquals("opcoes", UtilImpl.unidecode("opções"));
		Assert.assertEquals("endereco", UtilImpl.unidecode("endereço"));
		Assert.assertEquals("LocalDeInstalacao", UtilImpl.unidecode("LocalDeInstalação"));
		Assert.assertEquals("kayttaja", UtilImpl.unidecode("käyttäjä"));
		Assert.assertEquals("flagObbligatorieta", UtilImpl.unidecode("flagObbligatorietà"));
		Assert.assertEquals("namenserganzung", UtilImpl.unidecode("namensergänzung"));
		Assert.assertEquals("prenom", UtilImpl.unidecode("prénom"));
		Assert.assertEquals("data", UtilImpl.unidecode("дата"));
		Assert.assertEquals("prioritat", UtilImpl.unidecode("priorität"));
		Assert.assertEquals("Hebergement", UtilImpl.unidecode("Hébergement"));
		Assert.assertEquals("filDactualite", UtilImpl.unidecode("filDactualité"));
		Assert.assertEquals("capacite", UtilImpl.unidecode("capacité"));
		Assert.assertEquals("endereco", UtilImpl.unidecode("endereço"));
		Assert.assertEquals("escritorio", UtilImpl.unidecode("escritório"));
		Assert.assertEquals("noRecu", UtilImpl.unidecode("noReçu"));
		Assert.assertEquals("yeuCauTuyenDung", UtilImpl.unidecode("yêuCầuTuyểnDụng"));
		Assert.assertEquals("heureDarrivee", UtilImpl.unidecode("heureDarrivée"));
	}
}
