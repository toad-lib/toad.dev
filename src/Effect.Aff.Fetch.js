export optionsToMilkOptions = isJust => fromJust => ({body, credentials, redirect, method, headers}) => {
  const obj = {method}

  if (isJust(body)) obj['body'] = fromJust(body)
  if (isJust(credentials)) obj['credentials'] = fromJust(credentials)
  if (isJust(redirect)) obj['redirect'] = fromJust(redirect)
  if (isJust(headers)) obj['headers'] = fromJust(headers)
                                          .reduce( (o, {key, value}) => {
                                                     o[key] = value
                                                     return o
                                                   }
                                                 , {}
                                                 )

  return obj
}
