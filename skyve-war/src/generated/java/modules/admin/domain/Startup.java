package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlEnum;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import modules.admin.Country.CountryExtension;
import modules.admin.Startup.StartupExtension;
import org.locationtech.jts.geom.Geometry;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;
import org.skyve.impl.domain.types.jaxb.GeometryMapper;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

/**
 * Startup Configuration
 * <br/>
 * This document provides a visual way to edit the JSON configuration
		and is shown to the administrator by default on first login.
 * 
 * @depend - - - MapType
 * @depend - - - CaptchaType
 * @depend - - - GeoIPCountryListType
 * @depend - - - BackupType
 * @navhas n geoIPCountries 0..n Country
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class Startup extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "Startup";

	/** @hidden */
	public static final String dontShowAgainPropertyName = "dontShowAgain";

	/** @hidden */
	public static final String environmentIdentifierPropertyName = "environmentIdentifier";

	/** @hidden */
	public static final String environmentSupportEmailPropertyName = "environmentSupportEmail";

	/** @hidden */
	public static final String mapTypePropertyName = "mapType";

	/** @hidden */
	public static final String mapZoomPropertyName = "mapZoom";

	/** @hidden */
	public static final String mapLayerPropertyName = "mapLayer";

	/** @hidden */
	public static final String mapCentrePropertyName = "mapCentre";

	/** @hidden */
	public static final String mailServerUrlPropertyName = "mailServerUrl";

	/** @hidden */
	public static final String mailPortPropertyName = "mailPort";

	/** @hidden */
	public static final String mailUsernamePropertyName = "mailUsername";

	/** @hidden */
	public static final String mailPasswordPropertyName = "mailPassword";

	/** @hidden */
	public static final String mailSenderPropertyName = "mailSender";

	/** @hidden */
	public static final String mailBogusSendPropertyName = "mailBogusSend";

	/** @hidden */
	public static final String mailTestRecipientPropertyName = "mailTestRecipient";

	/** @hidden */
	public static final String checkForBreachedPasswordPropertyName = "checkForBreachedPassword";

	/** @hidden */
	public static final String captchaTypePropertyName = "captchaType";

	/** @hidden */
	public static final String apiGoogleMapsKeyPropertyName = "apiGoogleMapsKey";

	/** @hidden */
	public static final String apiGoogleRecaptchaSiteKeyPropertyName = "apiGoogleRecaptchaSiteKey";

	/** @hidden */
	public static final String apiGoogleRecaptchaSecretKeyPropertyName = "apiGoogleRecaptchaSecretKey";

	/** @hidden */
	public static final String apiCloudflareTurnstileSiteKeyPropertyName = "apiCloudflareTurnstileSiteKey";

	/** @hidden */
	public static final String apiCloudflareTurnstileSecretKeyPropertyName = "apiCloudflareTurnstileSecretKey";

	/** @hidden */
	public static final String geoIPKeyPropertyName = "geoIPKey";

	/** @hidden */
	public static final String geoIPCountryListTypePropertyName = "geoIPCountryListType";

	/** @hidden */
	public static final String geoIPCountriesPropertyName = "geoIPCountries";

	/** @hidden */
	public static final String accountAllowUserSelfRegistrationPropertyName = "accountAllowUserSelfRegistration";

	/** @hidden */
	public static final String apiTwilioSIDPropertyName = "apiTwilioSID";

	/** @hidden */
	public static final String apiTwilioAuthTokenPropertyName = "apiTwilioAuthToken";

	/** @hidden */
	public static final String apiTwilioDefaultSendNumberPropertyName = "apiTwilioDefaultSendNumber";

	/** @hidden */
	public static final String backupTypePropertyName = "backupType";

	/** @hidden */
	public static final String backupConnectionStringPropertyName = "backupConnectionString";

	/** @hidden */
	public static final String backupDirectoryNamePropertyName = "backupDirectoryName";

	/**
	 * Type
	 * <br/>
	 * Which map technology would you like to use for this Skyve application? Note: Google Maps requires an API key.
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum MapType implements Enumeration {
		gmap("gmap", "Google Maps"),
		leaflet("leaflet", "Open Street Map");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(MapType::toDomainValue).collect(Collectors.toUnmodifiableList());

		private MapType(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static MapType fromCode(String code) {
			MapType result = null;

			for (MapType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static MapType fromLocalisedDescription(String description) {
			MapType result = null;

			for (MapType value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * CAPTCHA Type
	 * <br/>
	 * To enable the forgot password function, this application must be registered for a captcha service. You may choose between Cloudflare Turnstile and Google Recaptcha.
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum CaptchaType implements Enumeration {
		googleRecaptcha("Google Recaptcha", "Google Recaptcha"),
		cloudflareTurnstile("Cloudflare Turnstile", "Cloudflare Turnstile");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(CaptchaType::toDomainValue).collect(Collectors.toUnmodifiableList());

		private CaptchaType(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static CaptchaType fromCode(String code) {
			CaptchaType result = null;

			for (CaptchaType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static CaptchaType fromLocalisedDescription(String description) {
			CaptchaType result = null;

			for (CaptchaType value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * Country List Type
	 * <br/>
	 * This determines whether the countries selected should be allowed (whitelist) or denied (blacklist) from accessing the application.
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum GeoIPCountryListType implements Enumeration {
		blacklist("blacklist", "Blacklist"),
		whitelist("whitelist", "Whitelist");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(GeoIPCountryListType::toDomainValue).collect(Collectors.toUnmodifiableList());

		private GeoIPCountryListType(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static GeoIPCountryListType fromCode(String code) {
			GeoIPCountryListType result = null;

			for (GeoIPCountryListType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static GeoIPCountryListType fromLocalisedDescription(String description) {
			GeoIPCountryListType result = null;

			for (GeoIPCountryListType value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * Type
	 * <br/>
	 * Which external backup provider should be used this Skyve application? Note: additional charges may apply.
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum BackupType implements Enumeration {
		none("none", "None (Internal Backups)"),
		azure("org.skyve.impl.backup.AzureBlobStorageBackup", "Azure Blob Storage");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(BackupType::toDomainValue).collect(Collectors.toUnmodifiableList());

		private BackupType(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static BackupType fromCode(String code) {
			BackupType result = null;

			for (BackupType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static BackupType fromLocalisedDescription(String description) {
			BackupType result = null;

			for (BackupType value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * Don't show this again
	 * <br/>
	 * Allow the user to bypass the setup screen and set the showSetup value to false.
	 **/
	private Boolean dontShowAgain = Boolean.valueOf(false);

	/**
	 * Identifier
	 * <br/>
	 * The description of this environment, e.g. Test, UAT. Leave this blank for production.
					<br/>
					<em>Note: If this is blank, the bootstrap user (if configured) will not be created for 
					this instance until this is set to a non-blank value.</em>
	 **/
	private String environmentIdentifier;

	/**
	 * Support Email Address
	 * <br/>
	 * Email address for system support
	 **/
	private String environmentSupportEmail;

	/**
	 * Type
	 * <br/>
	 * Which map technology would you like to use for this Skyve application? Note: Google Maps requires an API key.
	 **/
	private MapType mapType = MapType.leaflet;

	/**
	 * Zoom
	 * <br/>
	 * What should the default zoom level be when opening a new map (value between 1-19)?
	 **/
	private Integer mapZoom = Integer.valueOf(1);

	/**
	 * Layers
	 * <br/>
	 * Google Map or Leaflet layer to show the map backdrop
	 **/
	private String mapLayer;

	/**
	 * Centre
	 * <br/>
	 * Where to centre a new map when it opens
	 **/
	private Geometry mapCentre;

	/**
	 * Server URL
	 * <br/>
	 * URL or IP address of the SMTP server to use
	 **/
	private String mailServerUrl;

	/**
	 * Server Port
	 * <br/>
	 * Which port should be used to access the mail server? This is usually 25, 465 or 587 depending if it is secure or insecure.
	 **/
	private Integer mailPort = Integer.valueOf(25);

	/**
	 * Username
	 * <br/>
	 * Mail server username
	 **/
	private String mailUsername;

	/**
	 * Password
	 * <br/>
	 * Mail server password
	 **/
	private String mailPassword;

	/**
	 * Default Sender
	 * <br/>
	 * Default send from email address
	 **/
	private String mailSender;

	/**
	 * Test Mode
	 * <br/>
	 * If true, email is disabled and just logged, it will never be sent
	 **/
	private Boolean mailBogusSend = Boolean.valueOf(false);

	/**
	 * Test Mail Recipient
	 * <br/>
	 * All emails will only be sent to this email address if specified
	 **/
	private String mailTestRecipient;

	/**
	 * Check for breached password
	 * <br/>
	 * When users try to create or change a password, this checks whether the new password has been compromised in known data breaches (requires internet access).
	 * <br/>
	 * Determines whether or not HaveIBeenPwned API is used as part of password validation.
	 **/
	private Boolean checkForBreachedPassword = Boolean.valueOf(true);

	/**
	 * CAPTCHA Type
	 * <br/>
	 * To enable the forgot password function, this application must be registered for a captcha service. You may choose between Cloudflare Turnstile and Google Recaptcha.
	 **/
	private CaptchaType captchaType;

	/**
	 * Google Maps Key
	 * <br/>
	 * If using Google Maps for your map type, specify your map key here
	 **/
	private String apiGoogleMapsKey;

	/**
	 * Google Recaptcha Site Key
	 * <br/>
	 * Google Recaptcha site key must be specified here.
	 **/
	private String apiGoogleRecaptchaSiteKey;

	/**
	 * Google Recaptcha Secret Key
	 * <br/>
	 * Google Recaptcha secret key can be specified here to enable server-side validation for stronger security.
	 **/
	private String apiGoogleRecaptchaSecretKey;

	/**
	 * Cloudflare Turnstile Site Key
	 * <br/>
	 * Cloudflare Turnstile site key must be specified here.
	 **/
	private String apiCloudflareTurnstileSiteKey;

	/**
	 * Cloudflare Turnstile Secret Key
	 * <br/>
	 * Cloudflare Turnstile secret key can be specified here to enable server-side validation for stronger security.
	 **/
	private String apiCloudflareTurnstileSecretKey;

	/**
	 * Geo IP Key/Token
	 * <br/>
	 * By supplying a Geo IP API token (default is ipinfo.io), you can allow/disallow countries for registration and password reset.
	 **/
	private String geoIPKey;

	/**
	 * Country List Type
	 * <br/>
	 * This determines whether the countries selected should be allowed (whitelist) or denied (blacklist) from accessing the application.
	 **/
	private GeoIPCountryListType geoIPCountryListType;

	/**
	 * Counties
	 **/
	private List<CountryExtension> geoIPCountries = new ChangeTrackingArrayList<>("geoIPCountries", this);

	/**
	 * Allow User Self Registration
	 * <br/>
	 * Allows new users to register for an account when enabled, requires email.
	 **/
	private Boolean accountAllowUserSelfRegistration = Boolean.valueOf(false);

	/**
	 * Account SID
	 **/
	private String apiTwilioSID;

	/**
	 * Account Auth Token
	 **/
	private String apiTwilioAuthToken;

	/**
	 * Default Send Number
	 **/
	private String apiTwilioDefaultSendNumber;

	/**
	 * Type
	 * <br/>
	 * Which external backup provider should be used this Skyve application? Note: additional charges may apply.
	 **/
	private BackupType backupType = BackupType.none;

	/**
	 * Connection String
	 * <br/>
	 * The connection string to the external backup location, e.g. 
					<code style='white-space: pre-wrap;'>DefaultEndpointsProtocol=https;AccountName=ACCOUNT_NAME;AccountKey=ACCOUNT_KEY;EndpointSuffix=core.windows.net</code>.
	 **/
	private String backupConnectionString;

	/**
	 * Directory Name
	 * <br/>
	 * The name of the top-level backup directory, e.g. <code>application-name</code>, this will be 
					created if it does not exist.<br/>
					This must be a valid DNS name, starting with a letter or number, containing only letters, numbers
					and the dash character. Every dash must be immediately preceeded and followed by a ltter or number.<br/>
					Must be from 3 to 63 characters long.
	 **/
	private String backupDirectoryName;

	@Override
	@XmlTransient
	public String getBizModule() {
		return Startup.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Startup.DOCUMENT_NAME;
	}

	public static StartupExtension newInstance() {
		try {
			return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
		}
		catch (RuntimeException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		return toString();

	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Startup) && 
					this.getBizId().equals(((Startup) o).getBizId()));
	}

	/**
	 * {@link #dontShowAgain} accessor.
	 * @return	The value.
	 **/
	public Boolean getDontShowAgain() {
		return dontShowAgain;
	}

	/**
	 * {@link #dontShowAgain} mutator.
	 * @param dontShowAgain	The new value.
	 **/
	@XmlElement
	public void setDontShowAgain(Boolean dontShowAgain) {
		preset(dontShowAgainPropertyName, dontShowAgain);
		this.dontShowAgain = dontShowAgain;
	}

	/**
	 * {@link #environmentIdentifier} accessor.
	 * @return	The value.
	 **/
	public String getEnvironmentIdentifier() {
		return environmentIdentifier;
	}

	/**
	 * {@link #environmentIdentifier} mutator.
	 * @param environmentIdentifier	The new value.
	 **/
	@XmlElement
	public void setEnvironmentIdentifier(String environmentIdentifier) {
		preset(environmentIdentifierPropertyName, environmentIdentifier);
		this.environmentIdentifier = environmentIdentifier;
	}

	/**
	 * {@link #environmentSupportEmail} accessor.
	 * @return	The value.
	 **/
	public String getEnvironmentSupportEmail() {
		return environmentSupportEmail;
	}

	/**
	 * {@link #environmentSupportEmail} mutator.
	 * @param environmentSupportEmail	The new value.
	 **/
	@XmlElement
	public void setEnvironmentSupportEmail(String environmentSupportEmail) {
		preset(environmentSupportEmailPropertyName, environmentSupportEmail);
		this.environmentSupportEmail = environmentSupportEmail;
	}

	/**
	 * {@link #mapType} accessor.
	 * @return	The value.
	 **/
	public MapType getMapType() {
		return mapType;
	}

	/**
	 * {@link #mapType} mutator.
	 * @param mapType	The new value.
	 **/
	@XmlElement
	public void setMapType(MapType mapType) {
		preset(mapTypePropertyName, mapType);
		this.mapType = mapType;
	}

	/**
	 * {@link #mapZoom} accessor.
	 * @return	The value.
	 **/
	public Integer getMapZoom() {
		return mapZoom;
	}

	/**
	 * {@link #mapZoom} mutator.
	 * @param mapZoom	The new value.
	 **/
	@XmlElement
	public void setMapZoom(Integer mapZoom) {
		preset(mapZoomPropertyName, mapZoom);
		this.mapZoom = mapZoom;
	}

	/**
	 * {@link #mapLayer} accessor.
	 * @return	The value.
	 **/
	public String getMapLayer() {
		return mapLayer;
	}

	/**
	 * {@link #mapLayer} mutator.
	 * @param mapLayer	The new value.
	 **/
	@XmlElement
	public void setMapLayer(String mapLayer) {
		preset(mapLayerPropertyName, mapLayer);
		this.mapLayer = mapLayer;
	}

	/**
	 * {@link #mapCentre} accessor.
	 * @return	The value.
	 **/
	public Geometry getMapCentre() {
		return mapCentre;
	}

	/**
	 * {@link #mapCentre} mutator.
	 * @param mapCentre	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(GeometryMapper.class)
	public void setMapCentre(Geometry mapCentre) {
		preset(mapCentrePropertyName, mapCentre);
		this.mapCentre = mapCentre;
	}

	/**
	 * {@link #mailServerUrl} accessor.
	 * @return	The value.
	 **/
	public String getMailServerUrl() {
		return mailServerUrl;
	}

	/**
	 * {@link #mailServerUrl} mutator.
	 * @param mailServerUrl	The new value.
	 **/
	@XmlElement
	public void setMailServerUrl(String mailServerUrl) {
		preset(mailServerUrlPropertyName, mailServerUrl);
		this.mailServerUrl = mailServerUrl;
	}

	/**
	 * {@link #mailPort} accessor.
	 * @return	The value.
	 **/
	public Integer getMailPort() {
		return mailPort;
	}

	/**
	 * {@link #mailPort} mutator.
	 * @param mailPort	The new value.
	 **/
	@XmlElement
	public void setMailPort(Integer mailPort) {
		preset(mailPortPropertyName, mailPort);
		this.mailPort = mailPort;
	}

	/**
	 * {@link #mailUsername} accessor.
	 * @return	The value.
	 **/
	public String getMailUsername() {
		return mailUsername;
	}

	/**
	 * {@link #mailUsername} mutator.
	 * @param mailUsername	The new value.
	 **/
	@XmlElement
	public void setMailUsername(String mailUsername) {
		preset(mailUsernamePropertyName, mailUsername);
		this.mailUsername = mailUsername;
	}

	/**
	 * {@link #mailPassword} accessor.
	 * @return	The value.
	 **/
	public String getMailPassword() {
		return mailPassword;
	}

	/**
	 * {@link #mailPassword} mutator.
	 * @param mailPassword	The new value.
	 **/
	@XmlElement
	public void setMailPassword(String mailPassword) {
		preset(mailPasswordPropertyName, mailPassword);
		this.mailPassword = mailPassword;
	}

	/**
	 * {@link #mailSender} accessor.
	 * @return	The value.
	 **/
	public String getMailSender() {
		return mailSender;
	}

	/**
	 * {@link #mailSender} mutator.
	 * @param mailSender	The new value.
	 **/
	@XmlElement
	public void setMailSender(String mailSender) {
		preset(mailSenderPropertyName, mailSender);
		this.mailSender = mailSender;
	}

	/**
	 * {@link #mailBogusSend} accessor.
	 * @return	The value.
	 **/
	public Boolean getMailBogusSend() {
		return mailBogusSend;
	}

	/**
	 * {@link #mailBogusSend} mutator.
	 * @param mailBogusSend	The new value.
	 **/
	@XmlElement
	public void setMailBogusSend(Boolean mailBogusSend) {
		preset(mailBogusSendPropertyName, mailBogusSend);
		this.mailBogusSend = mailBogusSend;
	}

	/**
	 * {@link #mailTestRecipient} accessor.
	 * @return	The value.
	 **/
	public String getMailTestRecipient() {
		return mailTestRecipient;
	}

	/**
	 * {@link #mailTestRecipient} mutator.
	 * @param mailTestRecipient	The new value.
	 **/
	@XmlElement
	public void setMailTestRecipient(String mailTestRecipient) {
		preset(mailTestRecipientPropertyName, mailTestRecipient);
		this.mailTestRecipient = mailTestRecipient;
	}

	/**
	 * {@link #checkForBreachedPassword} accessor.
	 * @return	The value.
	 **/
	public Boolean getCheckForBreachedPassword() {
		return checkForBreachedPassword;
	}

	/**
	 * {@link #checkForBreachedPassword} mutator.
	 * @param checkForBreachedPassword	The new value.
	 **/
	@XmlElement
	public void setCheckForBreachedPassword(Boolean checkForBreachedPassword) {
		preset(checkForBreachedPasswordPropertyName, checkForBreachedPassword);
		this.checkForBreachedPassword = checkForBreachedPassword;
	}

	/**
	 * {@link #captchaType} accessor.
	 * @return	The value.
	 **/
	public CaptchaType getCaptchaType() {
		return captchaType;
	}

	/**
	 * {@link #captchaType} mutator.
	 * @param captchaType	The new value.
	 **/
	@XmlElement
	public void setCaptchaType(CaptchaType captchaType) {
		preset(captchaTypePropertyName, captchaType);
		this.captchaType = captchaType;
	}

	/**
	 * {@link #apiGoogleMapsKey} accessor.
	 * @return	The value.
	 **/
	public String getApiGoogleMapsKey() {
		return apiGoogleMapsKey;
	}

	/**
	 * {@link #apiGoogleMapsKey} mutator.
	 * @param apiGoogleMapsKey	The new value.
	 **/
	@XmlElement
	public void setApiGoogleMapsKey(String apiGoogleMapsKey) {
		preset(apiGoogleMapsKeyPropertyName, apiGoogleMapsKey);
		this.apiGoogleMapsKey = apiGoogleMapsKey;
	}

	/**
	 * {@link #apiGoogleRecaptchaSiteKey} accessor.
	 * @return	The value.
	 **/
	public String getApiGoogleRecaptchaSiteKey() {
		return apiGoogleRecaptchaSiteKey;
	}

	/**
	 * {@link #apiGoogleRecaptchaSiteKey} mutator.
	 * @param apiGoogleRecaptchaSiteKey	The new value.
	 **/
	@XmlElement
	public void setApiGoogleRecaptchaSiteKey(String apiGoogleRecaptchaSiteKey) {
		preset(apiGoogleRecaptchaSiteKeyPropertyName, apiGoogleRecaptchaSiteKey);
		this.apiGoogleRecaptchaSiteKey = apiGoogleRecaptchaSiteKey;
	}

	/**
	 * {@link #apiGoogleRecaptchaSecretKey} accessor.
	 * @return	The value.
	 **/
	public String getApiGoogleRecaptchaSecretKey() {
		return apiGoogleRecaptchaSecretKey;
	}

	/**
	 * {@link #apiGoogleRecaptchaSecretKey} mutator.
	 * @param apiGoogleRecaptchaSecretKey	The new value.
	 **/
	@XmlElement
	public void setApiGoogleRecaptchaSecretKey(String apiGoogleRecaptchaSecretKey) {
		preset(apiGoogleRecaptchaSecretKeyPropertyName, apiGoogleRecaptchaSecretKey);
		this.apiGoogleRecaptchaSecretKey = apiGoogleRecaptchaSecretKey;
	}

	/**
	 * {@link #apiCloudflareTurnstileSiteKey} accessor.
	 * @return	The value.
	 **/
	public String getApiCloudflareTurnstileSiteKey() {
		return apiCloudflareTurnstileSiteKey;
	}

	/**
	 * {@link #apiCloudflareTurnstileSiteKey} mutator.
	 * @param apiCloudflareTurnstileSiteKey	The new value.
	 **/
	@XmlElement
	public void setApiCloudflareTurnstileSiteKey(String apiCloudflareTurnstileSiteKey) {
		preset(apiCloudflareTurnstileSiteKeyPropertyName, apiCloudflareTurnstileSiteKey);
		this.apiCloudflareTurnstileSiteKey = apiCloudflareTurnstileSiteKey;
	}

	/**
	 * {@link #apiCloudflareTurnstileSecretKey} accessor.
	 * @return	The value.
	 **/
	public String getApiCloudflareTurnstileSecretKey() {
		return apiCloudflareTurnstileSecretKey;
	}

	/**
	 * {@link #apiCloudflareTurnstileSecretKey} mutator.
	 * @param apiCloudflareTurnstileSecretKey	The new value.
	 **/
	@XmlElement
	public void setApiCloudflareTurnstileSecretKey(String apiCloudflareTurnstileSecretKey) {
		preset(apiCloudflareTurnstileSecretKeyPropertyName, apiCloudflareTurnstileSecretKey);
		this.apiCloudflareTurnstileSecretKey = apiCloudflareTurnstileSecretKey;
	}

	/**
	 * {@link #geoIPKey} accessor.
	 * @return	The value.
	 **/
	public String getGeoIPKey() {
		return geoIPKey;
	}

	/**
	 * {@link #geoIPKey} mutator.
	 * @param geoIPKey	The new value.
	 **/
	@XmlElement
	public void setGeoIPKey(String geoIPKey) {
		preset(geoIPKeyPropertyName, geoIPKey);
		this.geoIPKey = geoIPKey;
	}

	/**
	 * {@link #geoIPCountryListType} accessor.
	 * @return	The value.
	 **/
	public GeoIPCountryListType getGeoIPCountryListType() {
		return geoIPCountryListType;
	}

	/**
	 * {@link #geoIPCountryListType} mutator.
	 * @param geoIPCountryListType	The new value.
	 **/
	@XmlElement
	public void setGeoIPCountryListType(GeoIPCountryListType geoIPCountryListType) {
		preset(geoIPCountryListTypePropertyName, geoIPCountryListType);
		this.geoIPCountryListType = geoIPCountryListType;
	}

	/**
	 * {@link #geoIPCountries} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<CountryExtension> getGeoIPCountries() {
		return geoIPCountries;
	}

	/**
	 * {@link #geoIPCountries} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public CountryExtension getGeoIPCountriesElementById(String bizId) {
		return getElementById(geoIPCountries, bizId);
	}

	/**
	 * {@link #geoIPCountries} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setGeoIPCountriesElementById(String bizId, CountryExtension element) {
		setElementById(geoIPCountries, element);
	}

	/**
	 * {@link #geoIPCountries} add.
	 * @param element	The element to add.
	 **/
	public boolean addGeoIPCountriesElement(CountryExtension element) {
		return geoIPCountries.add(element);
	}

	/**
	 * {@link #geoIPCountries} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addGeoIPCountriesElement(int index, CountryExtension element) {
		geoIPCountries.add(index, element);
	}

	/**
	 * {@link #geoIPCountries} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeGeoIPCountriesElement(CountryExtension element) {
		return geoIPCountries.remove(element);
	}

	/**
	 * {@link #geoIPCountries} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public CountryExtension removeGeoIPCountriesElement(int index) {
		return geoIPCountries.remove(index);
	}

	/**
	 * {@link #accountAllowUserSelfRegistration} accessor.
	 * @return	The value.
	 **/
	public Boolean getAccountAllowUserSelfRegistration() {
		return accountAllowUserSelfRegistration;
	}

	/**
	 * {@link #accountAllowUserSelfRegistration} mutator.
	 * @param accountAllowUserSelfRegistration	The new value.
	 **/
	@XmlElement
	public void setAccountAllowUserSelfRegistration(Boolean accountAllowUserSelfRegistration) {
		preset(accountAllowUserSelfRegistrationPropertyName, accountAllowUserSelfRegistration);
		this.accountAllowUserSelfRegistration = accountAllowUserSelfRegistration;
	}

	/**
	 * {@link #apiTwilioSID} accessor.
	 * @return	The value.
	 **/
	public String getApiTwilioSID() {
		return apiTwilioSID;
	}

	/**
	 * {@link #apiTwilioSID} mutator.
	 * @param apiTwilioSID	The new value.
	 **/
	@XmlElement
	public void setApiTwilioSID(String apiTwilioSID) {
		preset(apiTwilioSIDPropertyName, apiTwilioSID);
		this.apiTwilioSID = apiTwilioSID;
	}

	/**
	 * {@link #apiTwilioAuthToken} accessor.
	 * @return	The value.
	 **/
	public String getApiTwilioAuthToken() {
		return apiTwilioAuthToken;
	}

	/**
	 * {@link #apiTwilioAuthToken} mutator.
	 * @param apiTwilioAuthToken	The new value.
	 **/
	@XmlElement
	public void setApiTwilioAuthToken(String apiTwilioAuthToken) {
		preset(apiTwilioAuthTokenPropertyName, apiTwilioAuthToken);
		this.apiTwilioAuthToken = apiTwilioAuthToken;
	}

	/**
	 * {@link #apiTwilioDefaultSendNumber} accessor.
	 * @return	The value.
	 **/
	public String getApiTwilioDefaultSendNumber() {
		return apiTwilioDefaultSendNumber;
	}

	/**
	 * {@link #apiTwilioDefaultSendNumber} mutator.
	 * @param apiTwilioDefaultSendNumber	The new value.
	 **/
	@XmlElement
	public void setApiTwilioDefaultSendNumber(String apiTwilioDefaultSendNumber) {
		preset(apiTwilioDefaultSendNumberPropertyName, apiTwilioDefaultSendNumber);
		this.apiTwilioDefaultSendNumber = apiTwilioDefaultSendNumber;
	}

	/**
	 * {@link #backupType} accessor.
	 * @return	The value.
	 **/
	public BackupType getBackupType() {
		return backupType;
	}

	/**
	 * {@link #backupType} mutator.
	 * @param backupType	The new value.
	 **/
	@XmlElement
	public void setBackupType(BackupType backupType) {
		preset(backupTypePropertyName, backupType);
		this.backupType = backupType;
	}

	/**
	 * {@link #backupConnectionString} accessor.
	 * @return	The value.
	 **/
	public String getBackupConnectionString() {
		return backupConnectionString;
	}

	/**
	 * {@link #backupConnectionString} mutator.
	 * @param backupConnectionString	The new value.
	 **/
	@XmlElement
	public void setBackupConnectionString(String backupConnectionString) {
		preset(backupConnectionStringPropertyName, backupConnectionString);
		this.backupConnectionString = backupConnectionString;
	}

	/**
	 * {@link #backupDirectoryName} accessor.
	 * @return	The value.
	 **/
	public String getBackupDirectoryName() {
		return backupDirectoryName;
	}

	/**
	 * {@link #backupDirectoryName} mutator.
	 * @param backupDirectoryName	The new value.
	 **/
	@XmlElement
	public void setBackupDirectoryName(String backupDirectoryName) {
		preset(backupDirectoryNamePropertyName, backupDirectoryName);
		this.backupDirectoryName = backupDirectoryName;
	}

	/**
	 * True when the selected backup type is Azure Blob Storage
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isBackupTypeAzure() {
		return (BackupType.azure == getBackupType());
	}

	/**
	 * {@link #isBackupTypeAzure} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotBackupTypeAzure() {
		return (! isBackupTypeAzure());
	}

	/**
	 * True when the captcha type is Cloudflare Turnstile
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isCloudflareTurnstile() {
		return (getCaptchaType() != null && CaptchaType.cloudflareTurnstile == getCaptchaType());
	}

	/**
	 * {@link #isCloudflareTurnstile} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotCloudflareTurnstile() {
		return (! isCloudflareTurnstile());
	}

	/**
	 * True when the captcha type is Google Recaptcha
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isGoogleRecaptcha() {
		return (getCaptchaType() != null && CaptchaType.googleRecaptcha == getCaptchaType());
	}

	/**
	 * {@link #isGoogleRecaptcha} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotGoogleRecaptcha() {
		return (! isGoogleRecaptcha());
	}

	/**
	 * True when a Geo IP key/token has been set
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isHasGeoIPKey() {
		return (geoIPKey != null);
	}

	/**
	 * {@link #isHasGeoIPKey} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotHasGeoIPKey() {
		return (! isHasGeoIPKey());
	}

	/**
	 * True when the selected map type is Google Maps
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isMapTypeGmap() {
		return (MapType.gmap == getMapType());
	}

	/**
	 * {@link #isMapTypeGmap} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotMapTypeGmap() {
		return (! isMapTypeGmap());
	}

	/**
	 * True when no captcha type is selected
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isNoCaptcha() {
		return (getCaptchaType() == null);
	}

	/**
	 * {@link #isNoCaptcha} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotNoCaptcha() {
		return (! isNoCaptcha());
	}

	/**
	 * True when this application has a default customer specified (is single tenant)
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isSingleTenant() {
		return (org.skyve.impl.util.UtilImpl.CUSTOMER != null);
	}

	/**
	 * {@link #isSingleTenant} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotSingleTenant() {
		return (! isSingleTenant());
	}
}
