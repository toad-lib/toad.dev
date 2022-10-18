export const unsafeWrapInShadow = el => {
  const shadow = el.attachShadow({mode: 'open'});

  const moveChildren = () => {
    Array.from(el.childNodes).forEach(c => shadow.appendChild(c));
  };

  (new MutationObserver(moveChildren)).observe(el, {childList: true});
  moveChildren();
};
