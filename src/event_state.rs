/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#[macro_export]
macro_rules! event_based_pseudo_classes {
    ($macro_name: ident) => { $macro_name! {
        #[doc = "The mouse is down on this element. \
                 (https://html.spec.whatwg.org/multipage/#selector-active). \
                 FIXME(#7333): set/unset this when appropriate"]
        event "active" => Active / IN_ACTIVE_STATE = 0x01,
        #[doc = "This element has focus.
                 https://html.spec.whatwg.org/multipage/scripting.html#selector-focus"]
        event "focus" => Focus / IN_FOCUS_STATE = 0x02,
        #[doc = "The mouse is hovering over this element. \
                 https://html.spec.whatwg.org/multipage/scripting.html#selector-hover"]
        event "hover" => Hover / IN_HOVER_STATE = 0x04,
        #[doc = "Content is enabled (and can be disabled). \
                 http://www.whatwg.org/html/#selector-enabled"]
        event "enabled" => Enabled / IN_ENABLED_STATE = 0x08,
        #[doc = "Content is disabled. \
                 http://www.whatwg.org/html/#selector-disabled"]
        event "disabled" => Disabled / IN_DISABLED_STATE = 0x10,
        #[doc = "Content is checked. \
                 https://html.spec.whatwg.org/multipage/scripting.html#selector-checked"]
        event "checked" => Checked / IN_CHECKED_STATE = 0x20,
        #[doc = "https://html.spec.whatwg.org/multipage/scripting.html#selector-indeterminate"]
        event "indeterminate" => Indeterminate / IN_INDETERMINATE_STATE = 0x40,
    }}
}

macro_rules! event_states_bitflag {
    ($(
        $(#[$Flag_attr: meta])*
        event $css: expr => $variant: ident / $flag: ident = $value: expr,
    )+) => {
        bitflags! {
            #[doc = "Element Event States."]
            flags EventState: u16 {
                $($(#[$Flag_attr])* const $flag = $value,)+
            }
        }
    }
}


event_based_pseudo_classes!(event_states_bitflag);
