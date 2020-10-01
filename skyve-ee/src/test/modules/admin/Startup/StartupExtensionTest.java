package modules.admin.Startup;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.assertThat;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;

import java.util.HashMap;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;

import modules.admin.domain.Startup.MapType;

public class StartupExtensionTest {

	Map<String, Object> overrideProperties;

	@Mock
	private Customer customer;

	@InjectMocks
	@Spy
	private StartupExtension bean;

	@Before
	public void before() throws Exception {
		MockitoAnnotations.initMocks(this);

		overrideProperties = new HashMap<>();
		UtilImpl.OVERRIDE_CONFIGURATION = overrideProperties;

		// set never null properties
		UtilImpl.SMTP = "localhost";
		UtilImpl.SMTP_SENDER = "test@test.com";
		UtilImpl.MAP_LAYERS = "google.maps.MapTypeId.ROADMAP";

		// mock the bean to return the default properties
		Mockito.when(bean.getMailServerUrl()).thenReturn(UtilImpl.SMTP);
		Mockito.when(bean.getMailSender()).thenReturn(UtilImpl.SMTP_SENDER);
		Mockito.when(bean.getMailPort()).thenReturn(Integer.valueOf(UtilImpl.SMTP_PORT));
		Mockito.when(bean.getMailBogusSend()).thenReturn(Boolean.valueOf(UtilImpl.SMTP_TEST_BOGUS_SEND));
		Mockito.when(bean.getMapLayer()).thenReturn(UtilImpl.MAP_LAYERS);
	}

	@Test
	public void testSaveConfigurationEmptyPropertiesWritesNulls() throws Exception {
		// setup mocks
		Mockito.doNothing().when(bean).writeConfiguration(anyString());

		// call the method under test
		bean.saveConfiguration();

		// verify
		Mockito.verify(bean, times(1)).writeConfiguration(anyString());
	}
	
	@Test
	public void testSaveConfigurationUpdatesApiProperties() throws Exception {
		// setup mocks
		Mockito.when(bean.getApiGoogleMapsKey()).thenReturn("12345");
		Mockito.when(bean.getApiGoogleRecaptchaKey()).thenReturn("12345");

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		Mockito.doNothing().when(bean).writeConfiguration(valueCapture.capture());

		// call the method under test
		bean.saveConfiguration();

		// verify the results
		Mockito.verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.API_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.API_GOOGLE_MAPS_V3_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.API_GOOGLE_RECAPTCHA_KEY));
	}

	@Test
	public void testSaveConfigurationUpdatesEnvironmentProperties() throws Exception {
		// setup mocks
		Mockito.when(bean.getEnvironmentIdentifier()).thenReturn("dev");
		Mockito.when(bean.getEnvironmentSupportEmail()).thenReturn("test@test.com");
		Mockito.when(bean.getDontShowAgain()).thenReturn(Boolean.TRUE);

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		Mockito.doNothing().when(bean).writeConfiguration(valueCapture.capture());

		// call the method under test
		bean.saveConfiguration();

		// verify the results
		Mockito.verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_IDENTIFIER_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_SHOW_SETUP_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_SUPPORT_EMAIL_ADDRESS_KEY));
	}

	@Test
	public void testSaveConfigurationUpdatesMailProperties() throws Exception {
		// setup mocks
		Mockito.when(bean.getMailServerUrl()).thenReturn("127.0.0.1");
		Mockito.when(bean.getMailSender()).thenReturn("test2@test.com");
		Mockito.when(bean.getMailPort()).thenReturn(Integer.valueOf(465));
		Mockito.when(bean.getMailUsername()).thenReturn("username");
		Mockito.when(bean.getMailPassword()).thenReturn("password");
		Mockito.when(bean.getMailBogusSend()).thenReturn(Boolean.TRUE);
		Mockito.when(bean.getMailTestRecipient()).thenReturn("test@test.com");

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		Mockito.doNothing().when(bean).writeConfiguration(valueCapture.capture());

		// call the method under test
		bean.saveConfiguration();

		// verify the results
		Mockito.verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_PORT_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_PWD_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_SENDER_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_SERVER_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_TEST_BOGUS_SEND_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_TEST_RECIPIENT_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_UID_KEY));
	}

	@Test
	public void testSaveConfigurationUpdatesMapProperties() throws Exception {
		// setup mocks
		Mockito.when(bean.getMapCentre()).thenReturn(new GeometryFactory().createPoint(new Coordinate(1, 1)));
		Mockito.when(bean.getMapLayer()).thenReturn("google.maps.MapTypeId.HYBRID");
		Mockito.when(bean.getMapType()).thenReturn(MapType.gmap);
		Mockito.when(bean.getMapZoom()).thenReturn(Integer.valueOf(19));

		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		Mockito.doNothing().when(bean).writeConfiguration(valueCapture.capture());

		// call the method under test
		bean.saveConfiguration();

		// verify the results
		Mockito.verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.MAP_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.MAP_CENTRE_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.MAP_LAYERS_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.MAP_TYPE_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.MAP_ZOOM_KEY));
	}

	@Test
	public void testSetDontShowEmptyOverrideProperties() throws Exception {
		// setup mocks
		Mockito.when(bean.getDontShowAgain()).thenReturn(Boolean.TRUE);
		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		Mockito.doNothing().when(bean).writeConfiguration(valueCapture.capture());
		
		// call the method under test
		bean.setDontShow();
		
		// verify
		Mockito.verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_SHOW_SETUP_KEY));
	}

	@Test
	public void testSetDontShowMergeExistingOverrideProperties() throws Exception {
		// setup test data
		Map<String, Object> smtp = new HashMap<>();
		smtp.put(StartupExtension.SMTP_SERVER_KEY, "localhost");
		overrideProperties.put(StartupExtension.SMTP_STANZA_KEY, smtp);

		// setup mocks
		Mockito.when(bean.getDontShowAgain()).thenReturn(Boolean.TRUE);
		ArgumentCaptor<String> valueCapture = ArgumentCaptor.forClass(String.class);
		Mockito.doNothing().when(bean).writeConfiguration(valueCapture.capture());

		// call the method under test
		bean.setDontShow();

		// verify
		Mockito.verify(bean, times(1)).writeConfiguration(anyString());
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_STANZA_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.ENVIRONMENT_SHOW_SETUP_KEY));
		assertThat(valueCapture.getValue(), containsString(StartupExtension.SMTP_STANZA_KEY));
	}

}
