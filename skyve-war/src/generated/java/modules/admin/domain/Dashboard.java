package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import java.util.ArrayList;
import java.util.List;
import modules.admin.Dashboard.DashboardExtension;
import modules.admin.DashboardWidget.DashboardWidgetExtension;
import modules.admin.User.UserExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;

/**
 * Dashboard
 * 
 * @navhas n favourites 0..n DashboardTile
 * @navhas n focusItem 0..1 DashboardWidget
 * @navhas n mentions 0..n Generic
 * @navhas n user 0..1 User
 * @navcomposed 1 dashboardWidgets 0..9 DashboardWidget
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class Dashboard extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "Dashboard";

	/** @hidden */
	public static final String userPropertyName = "user";

	/** @hidden */
	public static final String dashboardWidgetsPropertyName = "dashboardWidgets";

	/** @hidden */
	public static final String designModePropertyName = "designMode";

	/** @hidden */
	public static final String loadedPropertyName = "loaded";

	/** @hidden */
	public static final String focusItemPropertyName = "focusItem";

	/** @hidden */
	public static final String selectedExistingItemIdPropertyName = "selectedExistingItemId";

	/** @hidden */
	public static final String favouritesPropertyName = "favourites";

	/** @hidden */
	public static final String mentionsPropertyName = "mentions";

	/**
	 * User
	 **/
	private UserExtension user = null;

	/**
	 * Dashboard Widgets
	 **/
	private List<DashboardWidgetExtension> dashboardWidgets = new ChangeTrackingArrayList<>("dashboardWidgets", this);

	/**
	 * Design mode
	 **/
	private Boolean designMode = Boolean.valueOf(false);

	/**
	 * Loaded
	 **/
	private Boolean loaded = Boolean.valueOf(false);

	/**
	 * Select
	 **/
	private DashboardWidgetExtension focusItem = null;

	/**
	 * Selected item
	 **/
	private String selectedExistingItemId;

	/**
	 * Favourites
	 **/
	private List<DashboardTile> favourites = new ArrayList<>();

	/**
	 * Mentions
	 * <br/>
	 * Holds the collection of user conversation mentions when the widget is added to the dashboard.
	 **/
	private List<Generic> mentions = new ArrayList<>();

	@Override
	@XmlTransient
	public String getBizModule() {
		return Dashboard.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Dashboard.DOCUMENT_NAME;
	}

	public static DashboardExtension newInstance() {
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
		try {
			return org.skyve.util.Binder.formatMessage("Dashboard", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Dashboard) && 
					this.getBizId().equals(((Dashboard) o).getBizId()));
	}

	/**
	 * {@link #user} accessor.
	 * @return	The value.
	 **/
	public UserExtension getUser() {
		return user;
	}

	/**
	 * {@link #user} mutator.
	 * @param user	The new value.
	 **/
	@XmlElement
	public void setUser(UserExtension user) {
		if (this.user != user) {
			preset(userPropertyName, user);
			this.user = user;
		}
	}

	/**
	 * {@link #dashboardWidgets} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<DashboardWidgetExtension> getDashboardWidgets() {
		return dashboardWidgets;
	}

	/**
	 * {@link #dashboardWidgets} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public DashboardWidgetExtension getDashboardWidgetsElementById(String bizId) {
		return getElementById(dashboardWidgets, bizId);
	}

	/**
	 * {@link #dashboardWidgets} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setDashboardWidgetsElementById(String bizId, DashboardWidgetExtension element) {
		setElementById(dashboardWidgets, element);
	}

	/**
	 * {@link #dashboardWidgets} add.
	 * @param element	The element to add.
	 **/
	public boolean addDashboardWidgetsElement(DashboardWidgetExtension element) {
		boolean result = dashboardWidgets.add(element);
		if (result) {
			element.setParent((DashboardExtension) this);
		}
		return result;
	}

	/**
	 * {@link #dashboardWidgets} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addDashboardWidgetsElement(int index, DashboardWidgetExtension element) {
		dashboardWidgets.add(index, element);
		element.setParent((DashboardExtension) this);
	}

	/**
	 * {@link #dashboardWidgets} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeDashboardWidgetsElement(DashboardWidgetExtension element) {
		boolean result = dashboardWidgets.remove(element);
		if (result) {
			element.setParent(null);
		}
		return result;
	}

	/**
	 * {@link #dashboardWidgets} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public DashboardWidgetExtension removeDashboardWidgetsElement(int index) {
		DashboardWidgetExtension result = dashboardWidgets.remove(index);
		result.setParent(null);
		return result;
	}

	/**
	 * {@link #designMode} accessor.
	 * @return	The value.
	 **/
	public Boolean getDesignMode() {
		return designMode;
	}

	/**
	 * {@link #designMode} mutator.
	 * @param designMode	The new value.
	 **/
	@XmlElement
	public void setDesignMode(Boolean designMode) {
		this.designMode = designMode;
	}

	/**
	 * {@link #loaded} accessor.
	 * @return	The value.
	 **/
	public Boolean getLoaded() {
		return loaded;
	}

	/**
	 * {@link #loaded} mutator.
	 * @param loaded	The new value.
	 **/
	@XmlElement
	public void setLoaded(Boolean loaded) {
		this.loaded = loaded;
	}

	/**
	 * {@link #focusItem} accessor.
	 * @return	The value.
	 **/
	public DashboardWidgetExtension getFocusItem() {
		return focusItem;
	}

	/**
	 * {@link #focusItem} mutator.
	 * @param focusItem	The new value.
	 **/
	@XmlElement
	public void setFocusItem(DashboardWidgetExtension focusItem) {
		if (this.focusItem != focusItem) {
			this.focusItem = focusItem;
		}
	}

	/**
	 * {@link #selectedExistingItemId} accessor.
	 * @return	The value.
	 **/
	public String getSelectedExistingItemId() {
		return selectedExistingItemId;
	}

	/**
	 * {@link #selectedExistingItemId} mutator.
	 * @param selectedExistingItemId	The new value.
	 **/
	@XmlElement
	public void setSelectedExistingItemId(String selectedExistingItemId) {
		this.selectedExistingItemId = selectedExistingItemId;
	}

	/**
	 * {@link #favourites} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<DashboardTile> getFavourites() {
		return favourites;
	}

	/**
	 * {@link #favourites} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public DashboardTile getFavouritesElementById(String bizId) {
		return getElementById(favourites, bizId);
	}

	/**
	 * {@link #favourites} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setFavouritesElementById(String bizId, DashboardTile element) {
		setElementById(favourites, element);
	}

	/**
	 * {@link #favourites} add.
	 * @param element	The element to add.
	 **/
	public boolean addFavouritesElement(DashboardTile element) {
		return favourites.add(element);
	}

	/**
	 * {@link #favourites} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addFavouritesElement(int index, DashboardTile element) {
		favourites.add(index, element);
	}

	/**
	 * {@link #favourites} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeFavouritesElement(DashboardTile element) {
		return favourites.remove(element);
	}

	/**
	 * {@link #favourites} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public DashboardTile removeFavouritesElement(int index) {
		return favourites.remove(index);
	}

	/**
	 * {@link #mentions} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<Generic> getMentions() {
		return mentions;
	}

	/**
	 * {@link #mentions} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public Generic getMentionsElementById(String bizId) {
		return getElementById(mentions, bizId);
	}

	/**
	 * {@link #mentions} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setMentionsElementById(String bizId, Generic element) {
		setElementById(mentions, element);
	}

	/**
	 * {@link #mentions} add.
	 * @param element	The element to add.
	 **/
	public boolean addMentionsElement(Generic element) {
		return mentions.add(element);
	}

	/**
	 * {@link #mentions} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addMentionsElement(int index, Generic element) {
		mentions.add(index, element);
	}

	/**
	 * {@link #mentions} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeMentionsElement(Generic element) {
		return mentions.remove(element);
	}

	/**
	 * {@link #mentions} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public Generic removeMentionsElement(int index) {
		return mentions.remove(index);
	}

	/**
	 * Whether the Dashboard page is in design mode allowing dashboard personalisation
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isInDesignMode() {
		return (((DashboardExtension)this).inDesignMode());
	}

	/**
	 * {@link #isInDesignMode} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotInDesignMode() {
		return (! isInDesignMode());
	}

	/**
	 * Whether to show the button to add a new widget to the collection
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowAddWidgetAction() {
		return (((DashboardExtension)this).showAddWidgetAction());
	}

	/**
	 * {@link #isShowAddWidgetAction} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowAddWidgetAction() {
		return (! isShowAddWidgetAction());
	}

	/**
	 * Whether a custom chart is the focus of the activity
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowCustomChartOptions() {
		return (((DashboardExtension)this).showCustomChartOptions());
	}

	/**
	 * {@link #isShowCustomChartOptions} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowCustomChartOptions() {
		return (! isShowCustomChartOptions());
	}

	/**
	 * Whether to show the button to go to the selector page
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowGoToSelector() {
		return (((DashboardExtension)this).showGoToSelector());
	}

	/**
	 * {@link #isShowGoToSelector} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowGoToSelector() {
		return (! isShowGoToSelector());
	}
}